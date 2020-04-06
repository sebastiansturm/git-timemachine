;;; vc-timemachine.el --- Walk through git/hg revisions of a file

;; Copyright (C) 2014 Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Version: 4.11
;; URL: https://gitlab.com/pidu/git-timemachine
;; Keywords: vc
;; Package-Requires: ((emacs "24.3") (transient "0.1.0"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Use vc-timemachine to browse historic versions of a file with p
;;; (previous) and n (next).

;;; Code:

(require 'vc-git)
(require 'vc-hg)
(require 'cl-lib)
(require 'transient)

(defcustom vc-timemachine-abbreviation-length 12
 "Number of chars from the full sha1 hash to use for abbreviation."
 :type 'integer
 :group 'vc-timemachine)

(defcustom vc-timemachine-show-minibuffer-details t
 "Non-nil means that details of the commit (its hash and date)
will be shown in the minibuffer while navigating commits."
 :type 'boolean
 :group 'vc-timemachine)

(defface vc-timemachine-commit
 '((default :weight bold))
 "Face for vc timemachine commit sha"
 :group 'vc-timemachine)

(defface vc-timemachine-minibuffer-detail-face
  '((((class color) (background dark))
     :foreground "yellow")
    (((class color) (background light))
     :foreground "yellow4"))
  "How to display the minibuffer detail"
  :group 'vc-timemachine)

(defface vc-timemachine-minibuffer-author-face
  '((((class color) (background dark))
     :foreground "orange")
    (((class color) (background light))
     :foreground "DarkOrange4"))
  "How to display the author in minibuffer"
  :group 'vc-timemachine)

(defcustom vc-timemachine-minibuffer-detail
 'subject
 "What to display when `vc-timemachine-show-minibuffer-details` is t.
Available values are:
`commit` : The SHA hash of the commit
`subject`: The subject of the commit message"
 :type '(radio (const :tag "Commit SHA" commit) (const :tag "Commit Subject" subject))
 :group 'vc-timemachine)

(defcustom vc-timemachine-show-author
 t
 "Prepend author to minibuffer details."
 :type 'boolean
 :group 'vc-timemachine)

(defcustom vc-timemachine-global-arguments
 '((Git . ("-c" "log.showSignature=false" "--no-pager"))
   (Hg . ("--pager=off")))
 "Common arguments for all vc commands."
 :type 'list
 :group 'vc-timemachine)

(defcustom vc-timemachine-quit-to-invoking-buffer
 t
 "Switch to invoking buffer on ‘vc-timemachine-quit’."
 :type 'boolean
 :group 'vc-timemachine)

(defvar-local vc-timemachine-directory nil)
(defvar-local vc-timemachine-revision nil)
(defvar-local vc-timemachine-file nil)
(defvar-local vc-timemachine--revisions-cache nil)
(defvar-local vc-timemachine-backend nil)

(defun vc-timemachine-completing-read-fn (&rest args)
 "Apply ARGS to `ido-completing-read' if available and fall back to `completing-read'."
 (cond
  ((fboundp 'ivy-read) (apply 'ivy-read args))
  ((fboundp 'ido-completing-read) (apply 'ido-completing-read args))
  (t (apply 'completing-read args))))

(defun vc-timemachine--process-file (backend &rest args)
  "Run ‘process-file’ with ARGS and ‘vc-timemachine-global-arguments’ applied."
  (let* ((vc-program (alist-get backend `((Git . ,vc-git-program) (Hg . ,vc-hg-program))))
         (global-arguments (alist-get backend vc-timemachine-global-arguments)))
    (apply #'process-file vc-program nil t nil (append global-arguments args))))

(defun vc-timemachine--extract-hg-filename (file-name hg-rename-line)
  (or (let ((renames (split-string hg-rename-line "\0")))
        (cl-loop for rename in (split-string hg-rename-line "\0")
                 when (string-equal (car (split-string rename)) file-name)
                 return (replace-regexp-in-string "(\\|)" "" (cadr (split-string rename)))))
      ;; no rename found, filename should remain unchanged
      file-name))

(defun vc-timemachine--revisions (&optional vc-branch)
  "List git revisions of current buffers file.

When passed a VC-BRANCH, lists revisions from that branch."
  (if vc-timemachine--revisions-cache
      vc-timemachine--revisions-cache
    (setq vc-timemachine--revisions-cache
          (prog2
              (message "Fetching Revisions...")
              (let ((default-directory vc-timemachine-directory)
                    (file vc-timemachine-file)
                    (backend vc-timemachine-backend))
                (with-temp-buffer
                  (pcase-let
                      ((`(,pre ,maybe-branch ,post)
                        (alist-get
                         backend
                         `((Git
                            .
                            ((,backend "log")
                             ,(if vc-branch (list vc-branch) '())
                             ("--name-only" "--follow"
                              "--pretty=format:%H%x00%ar%x00%ad%x00%s%x00%an" "--" ,file)))
                           (Hg
                            .
                            ((,backend "log")
                             ,(if vc-branch (list "--branch" vc-branch) '())
                             ("--follow"
                              "--template"
                              "{node}\\0{date|age}\\0{date|rfc822date}\\0{desc|firstline}\\0{author}\\n{join(file_copies, \"\\0\")}\\n\\n"
                             ,file)))))))
                    (unless
                        (zerop
                         (apply #'vc-timemachine--process-file
                                (apply #'append (list pre maybe-branch post))))
                      (error "%s log command exited with non-zero exit status for file: %s"
                             (symbol-name backend) file)))

                  (goto-char (point-min))
                  (let ((lines)
                        (commit-number (/ (1+ (count-lines (point-min) (point-max))) 3))
                        ;; TODO: is there a way to have hg log output the target file's current
                        ;; path for every commit?
                        (file-name file))
                    (while (not (eobp))
                      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                        (string-match "\\([^\0]*\\)\0\\([^\0]*\\)\0\\([^\0]*\\)\0\\(.*\\)\0\\(.*\\)" line)
                        (let ((commit (match-string 1 line))
                              (date-relative (match-string 2 line))
                              (date-full (match-string 3 line))
                              (subject (match-string 4 line))
                              (author (match-string 5 line)))
                          (forward-line 1)
                          (let*
                              ((output-2nd-line
                                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                               (next-file-name
                                (pcase backend
                                  ;; git log output/2nd line: <filename>
                                  ('Git output-2nd-line)
                                  ;; hg log output/2nd line: <new name 1> (<old name 1>)<new name 2> (<old name 2>)...
                                  ('Hg (vc-timemachine--extract-hg-filename file-name output-2nd-line)))))
                            (push (list commit file-name commit-number date-relative date-full subject author) lines)
                            (setq file-name next-file-name))))
                      (setq commit-number (1- commit-number))
                      (forward-line 2))
                    (nreverse lines))))
            (message "Fetching Revisions...done")))))

(defun vc-timemachine-show-current-revision ()
  "Show last (current) revision of file."
  (interactive)
  (vc-timemachine-show-revision (car (vc-timemachine--revisions))))

(defun vc-timemachine-show-latest-revision-in-branch (vc-branch)
  "Show last (current) revision of file in VC-BRANCH."
  (interactive (format "M%s branch: " (symbol-name vc-timemachine-backend)))
  (vc-timemachine-show-revision (car (vc-timemachine--revisions vc-branch))))

(defun vc-timemachine--next-revision (revisions)
 "Return the revision following the current revision in REVISIONS."
 (or (cadr (cl-member (car vc-timemachine-revision) revisions :key #'car :test #'string=))
  (car (reverse revisions))))

(defun vc-timemachine--fuzzy-goto (current-line-contents)
  (let ((inhibit-field-text-motion t)
        (line-number (line-number-at-pos))
        (contents current-line-contents)
        (i 1)
        (penalty)
        (best-line-number)
        (best-penalty 1000000))
    (save-excursion
      (goto-char (point-min))
      (goto-char (line-end-position))
      (while (not (eq (point) (point-max)))
        (setq penalty
              (string-distance
               contents
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (when (or (< penalty best-penalty) (and (= penalty best-penalty)
                                                (< (abs (- line-number i))
                                                   (abs (- best-line-number i)))))
          (setq best-line-number i best-penalty penalty))
        (forward-line)
        (setq i (1+ i))))
    (forward-line (- best-line-number line-number))))

(defmacro vc-timemachine--following-line-do (curr-revision new-revision &rest body)
  `(if (eq vc-timemachine-backend 'Git)
       (progn
         (let ((new-line (vc-timemachine--find-new-current-line curr-revision new-revision (line-number-at-pos)))))
         ,@body
         (forward-line (- new-line (line-number-at-pos))))
     (let*
         ((inhibit-field-text-motion t)
          (current-line-contents (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
       ,@body
       (vc-timemachine--fuzzy-goto current-line-contents))))

(defun vc-timemachine-show-previous-revision ()
 "Show previous revision of file."
 (interactive)
 (let ((new-line nil)
       (curr-revision vc-timemachine-revision)
       (new-revision (vc-timemachine--next-revision (vc-timemachine--revisions)))
       (cursor-win-pos (vc-timemachine--get-cursor-position)))
   (vc-timemachine--following-line-do
    curr-revision new-revision
    (vc-timemachine-show-revision new-revision))
   (vc-timemachine--set-cursor-position cursor-win-pos)))

(defun vc-timemachine-show-next-revision ()
 "Show next revision of file."
 (interactive)
 (let ((new-line nil)
       (curr-revision vc-timemachine-revision)
       (new-revision (vc-timemachine--next-revision (reverse (vc-timemachine--revisions))))
       (cursor-win-pos (vc-timemachine--get-cursor-position)))
   (vc-timemachine--following-line-do
    curr-revision new-revision
    (vc-timemachine-show-revision new-revision))
  (vc-timemachine--set-cursor-position cursor-win-pos)))

(defun vc-timemachine-show-revision-fuzzy ()
 "Show the revision with the chosen commit message."
  (interactive)
 (let* ((revisions (vc-timemachine--revisions))
        (wanted
         (funcall #'vc-timemachine-completing-read-fn "Commit message: "
          (mapcar (apply-partially #'nth 5) revisions))))
  (vc-timemachine-show-revision
   (cl-find wanted revisions
    :key (apply-partially #'nth 5)
    :test #'equal))))

(defun vc-timemachine-show-nth-revision (rev-number)
  "Show the REV-NUMBER revision."
  (interactive "nEnter revision number: ")
  (let* ((revisions (reverse (vc-timemachine--revisions)))
         (num-revisions (length revisions))
         (curr-revision vc-timemachine-revision)
         (new-revision (nth (1- rev-number) revisions))
         (new-line nil)
         (cursor-win-pos (vc-timemachine--get-cursor-position)))
    (if (not new-revision)
        (message "Only %d revisions exist." num-revisions)
      (setq new-line
            (vc-timemachine--find-new-current-line curr-revision new-revision (line-number-at-pos)))
      (vc-timemachine-show-revision new-revision)
      (forward-line (- new-line (line-number-at-pos)))
      (vc-timemachine--set-cursor-position cursor-win-pos))))

(defun vc-timemachine-show-revision (revision)
 "Show a REVISION (commit hash) of the current file."
 (when revision
  (let ((current-position (point))
        (commit (car revision))
        (revision-file-name (nth 1 revision))
        (commit-index (nth 2 revision))
        (date-relative (nth 3 revision))
        (date-full (nth 4 revision))
        (subject (nth 5 revision)))
   (setq buffer-read-only nil)
   (erase-buffer)
   (let ((default-directory vc-timemachine-directory)
         (process-coding-system-alist (list (cons "" (cons buffer-file-coding-system default-process-coding-system)))))
     (pcase vc-timemachine-backend
       ('Git (vc-timemachine--process-file
              vc-timemachine-backend "show" (concat commit ":" revision-file-name)))
       ('Hg (vc-timemachine--process-file
             vc-timemachine-backend "cat" revision-file-name "-r" commit))))
   (setq buffer-read-only t)
   (set-buffer-modified-p nil)
   (let* ((revisions (vc-timemachine--revisions))
          (n-of-m (format "(%d/%d %s)" commit-index (length revisions) date-relative)))
    (setq mode-line-buffer-identification
     (list (propertized-buffer-identification "%12b") "@"
      (propertize (vc-timemachine-abbreviate commit) 'face 'vc-timemachine-commit) " name:" revision-file-name" " n-of-m)))
   (setq vc-timemachine-revision revision)
   (goto-char current-position)
   (when vc-timemachine-show-minibuffer-details
    (vc-timemachine--show-minibuffer-details revision))
   (vc-timemachine--erm-workaround))))

(defun vc-timemachine--erm-workaround ()
 "Workaround for enhanced ruby mode not detecting revision change."
 (when (eq major-mode 'enh-ruby-mode)
  (ignore-errors (erm-reset-buffer))))

(defun vc-timemachine--show-minibuffer-details (revision)
 "Show details for REVISION in minibuffer."
 (let* ((date-relative (nth 3 revision))
        (date-full (nth 4 revision))
        (author (if vc-timemachine-show-author (concat (nth 6 revision) ": ") ""))
        (sha-or-subject (if (eq vc-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
  (message "%s%s [%s (%s)]"
   (propertize author 'face 'vc-timemachine-minibuffer-author-face)
   (propertize sha-or-subject 'face 'vc-timemachine-minibuffer-detail-face) date-full date-relative)))

(defun vc-timemachine--blame (current-line current-commit new-commit file reverse)
  (when (eq vc-timemachine-backend 'Git) ; TODO: 'Hg support
    (pcase-let
        ((`(,pre ,post)
          (alist-get
           vc-timemachine-backend
           `((Git . ((,vc-timemachine-backend "blame")
                     ("-n"
                      ,(format "-L %s,%s" current-line current-line)
                      ,(format "%s..%s" current-commit new-commit)
                      "--" ,file)))))))
      (apply #'vc-timemachine--process-file
             (apply #'append (list pre (if reverse '("--reverse") '()) post)))
      (message "reverse: %s\n%s" reverse (buffer-substring-no-properties (point-min) (point-max))))))

(defun vc-timemachine--find-new-current-line (curr-revision new-revision current-line)
  "Return the new current line after a revision jump.

Given CURR-REVISION and NEW-REVISION determine if we need to updated CURRENT-LINE."
  (let* ((revisions (reverse (vc-timemachine--revisions)))
         (current-commit (car curr-revision))
         (curr-rev-number (+ (or (cl-position curr-revision revisions) 0) 1))
         (new-commit (car new-revision))
         (new-rev-number (+ (or (cl-position new-revision revisions) 0) 1))
         (new-line 0)
         (file vc-timemachine-file)
         (reverse (< curr-rev-number new-rev-number))
         (backend vc-timemachine-backend))
    ;; If no commit change, do nothing
    (if (= curr-rev-number new-rev-number)
        current-line
      ;; Get new current line number using `git-blame`
      (with-temp-buffer
        (setq vc-timemachine-backend backend)
        (vc-timemachine--blame current-line current-commit new-commit file reverse)

        (goto-char (point-min))
        ;; If end-of-buffer problem
        (when (search-forward-regexp "^fatal: file .+ has only .+ lines" nil t)
          (setq current-line (- current-line 1))
          (erase-buffer)
          (vc-timemachine--blame current-line current-commit new-commit file reverse))
        (goto-char (point-min))
        (when (eq vc-timemachine-backend 'Git)
          (search-forward-regexp "^[^ ]+ \\([^ ]+\\)")
          (setq new-line (string-to-number (match-string 1))))
         ; TODO
        ;; In case git blame doesn't give what we expect
        (when (= new-line 0) (setq new-line current-line))
        new-line))))

(defun vc-timemachine--get-cursor-position ()
  "Return the cursor visual line number with respect to the current window first line."
  (let* ((win-point-min (save-excursion (move-to-window-line 0) (point)))
         (cur-pos (count-screen-lines win-point-min (point))))
    cur-pos))

(defun vc-timemachine--set-cursor-position (POS)
  "Set the cursor position to the POS visual line with respect to the window first line."
  (recenter POS))

(defun vc-timemachine-abbreviate (revision)
 "Return REVISION abbreviated to `vc-timemachine-abbreviation-length' chars."
 (substring revision 0 vc-timemachine-abbreviation-length))

(defun vc-timemachine-quit ()
 "Exit the timemachine."
 (interactive)
 (let ((parent-buffer-name buffer-file-name))
  (kill-buffer)
  (let ((parent-buffer (find-buffer-visiting parent-buffer-name)))
   (when (and parent-buffer vc-timemachine-quit-to-invoking-buffer)
    (switch-to-buffer parent-buffer nil t)))))

(defun vc-timemachine--use-magit ()
  (and (eq vc-timemachine-backend 'Git) (fboundp 'magit-blame)))

(defun vc-timemachine-blame ()
 "Call ‘magit-blame’ on current revision, if the current file's VC backend is `Git' and magit is available, otherwise call `vc-annotate'."
 (interactive)
 (if (vc-timemachine--use-magit)
  (let ((magit-buffer-revision (car vc-timemachine-revision)))
   (magit-blame))
  (vc-annotate (buffer-file-name) (car vc-timemachine-revision))))

(defun vc-timemachine-kill-revision ()
 "Kill the current revisions abbreviated commit hash."
 (interactive)
 (let ((revision (car vc-timemachine-revision)))
  (message revision)
  (kill-new revision)))

(defun vc-timemachine-kill-abbreviated-revision ()
 "Kill the current revisions full commit hash."
 (interactive)
 (let ((revision (vc-timemachine-abbreviate (car vc-timemachine-revision))))
  (message revision)
  (kill-new revision)))

(defun vc-timemachine-show-commit ()
 "Show commit for current revision."
 (interactive)
 (let ((rev (car vc-timemachine-revision)))
  (if (vc-timemachine--use-magit)
   (magit-show-commit rev)
   (switch-to-buffer
    (vc-find-revision (buffer-file-name) rev)))))

(define-transient-command vc-timemachine-help ()
 "Show online help."
 ["Navigate"
  [("p" "show previous revision" vc-timemachine-show-previous-revision)
   ("n" "show next revision" vc-timemachine-show-next-revision)
   ("g" "show nth revision" vc-timemachine-show-nth-revision)
   ("t" "show fuzzy revision" vc-timemachine-show-revision-fuzzy)]]
 ["Kill current revision"
  [("w" "kill abbreviated revision" vc-timemachine-kill-abbreviated-revision)
   ("W" "kill revision" vc-timemachine-kill-revision)]]
 ["Misc"
  [("b" "blame current revision" vc-timemachine-blame)
   ("c" "show commit" vc-timemachine-show-commit)
   ("?" "show help" vc-timemachine-help)
   ("q" "quit" vc-timemachine-quit)]])

(define-minor-mode vc-timemachine-mode
 "VC Timemachine, feel the wings of history."
 :init-value nil
 :lighter " Timemachine"
 :keymap
 '(("p" . vc-timemachine-show-previous-revision)
   ("n" . vc-timemachine-show-next-revision)
   ("g" . vc-timemachine-show-nth-revision)
   ("t" . vc-timemachine-show-revision-fuzzy)
   ("q" . vc-timemachine-quit)
   ("w" . vc-timemachine-kill-abbreviated-revision)
   ("W" . vc-timemachine-kill-revision)
   ("b" . vc-timemachine-blame)
   ("c" . vc-timemachine-show-commit)
   ("?" . vc-timemachine-help))
 :group 'vc-timemachine)

(defun vc-timemachine-validate (file)
 "Validate that there is a FILE and that it belongs to a git repository.
Call with the value of 'buffer-file-name."
 (unless file
  (error "This buffer is not visiting a file"))
 (unless (or (vc-git-registered file) (vc-hg-registered file)
  (error "This file is not tracked by git or hg"))))

(defun vc-timemachine--start (get-revision-fn)
  "Setup a timemachine buffer and populate it from the result of GET-REVISION-FN."
  (setq vc-timemachine--revisions-cache nil)
  (vc-timemachine-validate (buffer-file-name))
  (let* ((file-name (buffer-file-name))
         (backend (vc-backend file-name))
         (root-fn (alist-get backend `((Git . ,#'vc-git-root) (Hg . ,#'vc-hg-root))))
         (directory (expand-file-name (funcall root-fn file-name)))
         (timemachine-buffer (format "timemachine:%s" (buffer-name)))
         (cur-line (line-number-at-pos))
         (cursor-position (vc-timemachine--get-cursor-position))
         (new-line nil)
         (mode major-mode)
         (coding-system buffer-file-coding-system))
    (with-current-buffer (get-buffer-create timemachine-buffer)
      (switch-to-buffer timemachine-buffer)
      (setq buffer-file-name file-name)
      (setq buffer-file-coding-system coding-system)
      (funcall mode)
      (setq vc-timemachine-directory directory
            vc-timemachine-file (file-relative-name file-name directory)
            vc-timemachine-revision nil
            vc-timemachine-backend backend)
      (funcall get-revision-fn)
      (setq new-line (vc-timemachine--find-new-current-line vc-timemachine-revision (list "HEAD" "" 0 "" "" "" "") cur-line)) ;; Allow to stay on the same line
      (goto-char (point-min))
      (forward-line (- new-line 1))
      (vc-timemachine--set-cursor-position cursor-position)
      (vc-timemachine-mode))))

;;;###autoload
(defun vc-timemachine-toggle ()
 "Toggle vc timemachine mode."
 (interactive)
 (if (bound-and-true-p vc-timemachine-mode)
  (vc-timemachine-quit)
  (vc-timemachine)))

;;;###autoload
(defun vc-timemachine ()
 "Enable vc timemachine for file of current buffer."
 (interactive)
 (vc-timemachine--start #'vc-timemachine-show-current-revision))

;;;###autoload
(defun vc-timemachine-switch-branch (vc-branch)
 "Enable vc timemachine for current buffer, switching to VC-BRANCH."
 ;; TODO: hg support
 (interactive (list (vc-timemachine-completing-read-fn "Branch to switch to: " (vc-git-branches))))
 (vc-timemachine--start (lambda () (vc-timemachine-show-latest-revision-in-branch vc-branch))))

(provide 'vc-timemachine)

;;; vc-timemachine.el ends here
