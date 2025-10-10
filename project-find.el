;; project-find.el --- Find files and content  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Geoff Jacobsen <geoffjacobsen@gmail.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; project-find is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; project-find is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; Unit-test-runner.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Package-Version: 0.1
;;
;;; Commentary:
;; Find files, content or anything in a interactive full buffer layout.
;;
;;; Code:

(require 'project)

(defgroup project-find nil
  "Find files and content."
  :group 'tools)

(defcustom pf-command "koru_find"
  "The server program that looks for files and content."
  :type 'string
  :group 'project-find)

(defcustom pf-find-project-open-commands (list #'pf 'magit-status #'dired)
  "Functions used to open a project first that exists is used."
  :type 'hook
  :group 'project-find)

(defconst pf-buffer-name "*project-find*"
  "Name of buffer for finding.")

(defvar-local pf-process-output ""
  "Buffer for process output.")

(defvar-local pf--base-filter ""
  "Text used as base filter.")

(defvar-local pf-filter-re ""
  "The Regexp used to filter `project-find' results.")

(defvar-local pf--filter-text ""
  "Text to filter the items found.")

(defvar-local pf--update-count 0
  "Count updates to buffer.")

(defgroup pf-faces nil
  "Faces used with utr."
  :group 'project-find)

(defface pf-common-prefix-face '((t :inherit font-lock-comment-delimiter-face :weight ultra-light))
  "Face for common result prefixes."
  :group 'pf-faces)

(defface pf-selected-face '((t :inherit secondary-selection))
  "Face for selected line."
  :group 'pf-faces)

(defface pf-filter-face '((t :inherit font-lock-regexp-face))
  "Face for filter text."
  :group 'pf-faces)

(defface pf-dir-face '((t :inherit font-lock-constant-face))
  "Face for filter text."
  :group 'pf-faces)

(defface pf-name-face '((t :inherit font-lock-operator-face))
  "Face for name of find command."
  :group 'pf-faces)

(defface pf-filter-prompt-face '((t :inherit font-lock-keyword-face))
  "Face for filter text."
  :group 'pf-faces)

(defconst pf--filter-properties
  (list
   'inhibit-read-only t
   'face 'pf-filter-face
   'line-prefix (propertize "Filter: " 'face 'pf-filter-prompt-face))
  "The prompt displayed before filter text.")

(defvar-local pf--name (propertize "Find file: " 'face 'pf-name-face)
  "The name of the type of find being performed.  See `pf-init'.")

(defvar-local pf-find-function nil
  "Custom find function called when an entry is selected.")

(defvar-local pf-filter-changed-function nil
  "Custom function to handle updating search results when the filter changes.")

(defvar-local pf--margin-overlays nil
  "Cache margin overlays.")

(defvar-local pf--selected-overlay nil
  "The overlay for the selected line.")

(defvar-local pf--dir-overlay nil
  "The overlay for the directory text.")

(defvar-local pf--selected-lineno 0
  "The selected line number.")

(defvar-keymap project-find-mode-map
  :doc "Keymap for `project-find-mode'."
  :parent text-mode-map
  "<remap> <self-insert-command>" #'pf-self-filter-add
  "<remap> <keyboard-quit>" #'pf-quit
  "<remap> <previous-line>" #'pf-backward-line
  "<remap> <next-line>" #'pf-forward-line
  "<backspace>" #'pf-backward-delete-char
  "<return>" #'pf-find-selected
  "M-1" #'pf-find-entry
  "M-2" #'pf-find-entry
  "M-3" #'pf-find-entry
  "M-4" #'pf-find-entry
  "M-5" #'pf-find-entry
  "M-6" #'pf-find-entry
  "M-7" #'pf-find-entry
  "M-8" #'pf-find-entry
  "M-9" #'pf-find-entry)

(define-derived-mode project-find-mode fundamental-mode "Project Find"
  ;;   "Major mode for finding project files.

  ;; \\{project-find-mode-map}"
  :group 'project-find

  ;;(add-hook 'context-menu-functions 'prog-context-menu 10 t)
  (setq buffer-read-only t
        pf--margin-overlays (pf--make-margin-overlays)
        pf--selected-overlay (make-overlay 1 1)
        pf--dir-overlay (make-overlay 1 1)
        bidi-paragraph-direction 'left-to-right)

  (setq-local after-change-functions '(pf-buffer-changed))

  (overlay-put pf--selected-overlay 'face 'pf-selected-face)
  (overlay-put pf--dir-overlay 'cursor-intangible t))

(defun pf-buffer-changed (_start _end _old-len)
  "Hook that sends any changes of the filter to the back-end server."
  (let ((text (pf-filter-text))
        (original pf--filter-text))
    (when (not (equal text pf--filter-text))
      (setq pf--filter-text text)
      (pf-propertize-filter-text)
      (if pf-filter-changed-function
          (funcall pf-filter-changed-function text original)
        (pf--process-send (format "set %d %s\x00" (length pf--base-filter) text))))))

(defun pf--process-send (data)
  "Send DATA to the project-find process."
  (process-send-string (get-buffer-process (get-buffer pf-buffer-name)) data))

(defun pf-process-filter (_proc output)
  "Display koru_find output.
OUTPUT contains data from the process."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (lines (split-string (concat pf-process-output output) "\x00"))
        ans)
    (while (cdr lines)
      (when-let* ((pl (pf-process-line (car lines))))
        (when (and pl (or (not ans) (< (car pl) (car ans))))
          (setq ans pl)))
      (setq lines (cdr lines)))
    (if ans
        (pf-post-process-filter (car ans) (cdr ans)))
    (setq pf-process-output (or (car lines) ""))))

(defun pf-process-line (line)
  "Handle a LINE of output from process."
  (setq pf--update-count (1+ pf--update-count))
  (let ((c (aref line 0)))
    (cond
     ((eq c ?c) (pf-clear-output))
     ((eq c ?+) (pf-add-line (substring line 1)))
     ((eq c ?-) (pf-rm-line (substring line 1)))
     ((eq c ?s) (ignore))
     ((eq c ?m) (progn (message "project-find: %s" line) nil))
     ((eq c ?d) (ignore))
     (t (error "Unknown command %S" line)))))

(defun pf-add-line (line)
  "Insert LINE into buffer.  Return pos and lineo of insertion."
  (with-current-buffer (pf-get-buffer-create)
    (save-excursion
      (pf-find-location line)
      (let ((start (point))
            (lineno (or (pf--margin-lineno-before-point) 10)))
        (insert line "\n")
        (pf--set-common-prefix)
        (when (not (eobp))
          (forward-line 1)
          (pf--set-common-prefix))
        (cons start lineno)))))

(defun pf-find-location (line)
  "Perform a binary search for LINE."
  (let ((key-start (overlay-end pf--dir-overlay))
        (key-end (point-max))
        (search-result nil)
        (mid-char-pos 0)
        (current-line-start 0)
        (current-line-end 0)
        (current-line-string 0))

    (while (< key-start key-end)
      (setq mid-char-pos (+ key-start (/ (- key-end key-start) 2))
            current-line-start (progn (goto-char mid-char-pos)
                                      (forward-line 0)
                                      (point))
            current-line-end (progn (goto-char mid-char-pos)
                                    (forward-line 1)
                                    (point))
            current-line-string (buffer-substring-no-properties
                                 current-line-start
                                 (1- current-line-end))
            search-result (compare-strings line nil nil
                                           current-line-string nil nil))

      (cond
       ((eq search-result t) ; search-result is 0: Found it!
        (setq key-start current-line-start
              key-end current-line-start))
       ((< search-result 0) ; Search string is LESS than current line
        (setq key-end current-line-start))

       (t ; Search string is GREATER than current line
        (setq key-start current-line-end))))
    (goto-char key-start)
    search-result))

(defun pf--margin-lineno-before-point ()
  "Get margin lineno at `point' if any."
  (let ((start (point)))
    (if (= (overlay-end pf--dir-overlay) start)
        0
      (forward-line -1)
      (goto-char start)

      (let ((ovs (overlays-in (point) (point)))
            lineno)
        (while ovs
          (setq lineno (overlay-get (car ovs) 'before-string))
          (if lineno
              (setq ovs nil
                    lineno (1- (string-to-number (cadr (get-text-property 0 'display lineno)))))
            (setq ovs (cdr ovs)))
          )
        lineno))))

(defun pf-rm-line (line)
  "Remove LINE from buffer.  Return pos and lineno of removal."
  (with-current-buffer (pf-get-buffer-create)
    (save-excursion
      (when (eq (pf-find-location line) t)
        (let* ((start (point))
               (lineno (or (pf--margin-lineno-before-point) 10)))
          (forward-line 1)
          (delete-region start (point))

          (goto-char start)
          (when (not (eobp))
            (forward-line 1)
            (pf--set-common-prefix))
          (cons start lineno))))))

(defun pf--find-common-dir (a b)
  "Find common directories between A and B.
A must be > B."
  (let ((cs (compare-strings a nil nil b nil nil)))
    (when (and (integerp cs) (> cs 1))
      (while (and (>= (setq cs (1- cs)) 0)
                  (/= (aref a cs) ?/)))
      (and (>= cs 0) (1+ cs)))))

(defun pf--set-common-prefix ()
  "Set the common prefix face compared to previous line."
  (save-excursion
    (let ((end (1- (point)))
          (start (progn (forward-line -1) (point)))
          (pstart (progn (forward-line -1) (point))))
      (remove-overlays start end 'face 'pf-common-prefix-face)
      (let ((cs (pf--find-common-dir (buffer-substring-no-properties start end)
                                     (buffer-substring-no-properties (1- start) pstart)))
            cov)
        (when (integerp cs)
          (setq cov (make-overlay start (+ start cs)))
          (overlay-put cov 'face 'pf-common-prefix-face))))))

(defun pf--wait-for (ready-test &optional time)
  "Wait TIME seconds for READY-TEST to return not nil.
TIME deaults to 0.5seconds.
READY-TEST is called from within `pf-buffer-name'.
Returns the reault of READY-TEST."
  (with-current-buffer (pf-get-buffer-create)
    (let ((until (+ (or time 0.5) (float-time)))
          ans)
      (while (and (> until (float-time))
                  (not (setq ans (funcall ready-test))))
        (sit-for 0.016))
      ans)))


(defun pf-post-process-filter (start lineno)
  "Set the margin counter from position START until LINENO is 10.
Reposition selected line."
  (save-excursion
    (let ((ppos start))
      (goto-char start)
      (while (and (< lineno 9) (not (eobp)))
        (pf--move-margin-overlay lineno (point))
        (setq ppos (point))
        (forward-line 1)
        (when (= lineno pf--selected-lineno)
          (move-overlay pf--selected-overlay ppos (point)))
        (setq lineno (1+ lineno)))

      (while (and (< lineno pf--selected-lineno) (not (eobp)))
        (setq ppos (point))
        (forward-line 1)
        (setq lineno (1+ lineno)))

      (when (and (<= lineno pf--selected-lineno))
        (if (= lineno 0)
            (delete-overlay pf--selected-overlay)
          (setq ppos (point))
          (forward-line -1)
          (move-overlay pf--selected-overlay (point) ppos)))

      (while (and (< lineno 9))
        (delete-overlay (aref pf--margin-overlays lineno))
        (setq lineno (1+ lineno))))))

(defun pf--move-margin-overlay (lineno pos)
  "Move cached margin overlay for LINENO to POS."
  (move-overlay (aref pf--margin-overlays lineno) pos pos))

(defun pf--make-margin-overlays ()
  "Initialize margin overlays for local buffer."
  (let ((a (make-vector 9 nil))
        (i 0))
    (while (< i 9)
      (let ((o (make-overlay 1 1))
            (n (propertize (format "%d" (1+ i))  'face 'default)))
        (overlay-put o 'before-string (propertize " " 'display `((margin left-margin) ,n)))
        (delete-overlay o)
        (aset a i o))
      (setq i (1+ i)))
    a))

(defun pf-find-entery-at (start end)
  "Find the entry location in the region START END.
If `pf-find-function' is not nil it will override the default behaviour.
The default behaviour will attempt to call `find-file-noselect'
on (`buffer-substring' START END).
This function must be called with `pf-buffer-name' as the current buffer."
  (if pf-find-function
      (funcall pf-find-function start end)
    (let ((filename (buffer-substring-no-properties start end)))
      (if (file-exists-p filename)
          (let ((buf (current-buffer))
                (nbuf (find-file-noselect filename)))
            (switch-to-prev-buffer nil 'kill)
            (switch-to-buffer nbuf nil t)
            (bury-buffer-internal buf))
        (error "File does not exist %s" filename)))))

(defun pf-selected-start ()
  "Return the buffer position of the start of the selected line."
  (overlay-start pf--selected-overlay))

(defun pf-selected-end ()
  "Return the buffer position of the end of the selected line."
  (when-let* ((end (overlay-end pf--selected-overlay)))
    (1- end)))

(defun pf-find-selected (&optional n)
  "Open buffer for the line relative to the selected line by N.
Opens the selected line if N is nil or 0."
  (interactive "P")
  (when n
    (pf-forward-line n))
  (when (overlay-buffer pf--selected-overlay)
    (pf-find-entery-at (pf-selected-start) (pf-selected-end))))

(defun pf-find-entry ()
  "Open buffer for file based on the basic key K."
  (interactive)
  (let ((n (- (event-basic-type last-command-event) ?0))
        start end)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1+ n))
      (setq start (point))
      (end-of-line)
      (setq end (point)))
    (pf-find-entery-at start end)))

(defun pf-process-sentinel (_process event)
  "Handle koru_find EVENT."
  (message "pf-process %s" event))

(defun pf-align-filter-input ()
  "Place cursor within filter input area."
  (when (>= (point) (overlay-start pf--dir-overlay))
    (goto-char (1- (overlay-start pf--dir-overlay)))))

(defun pf-filter-text ()
  "Return the text in the filter area."
  (if (overlay-buffer pf--dir-overlay)
      (buffer-substring-no-properties (point-min)
                                      (1- (overlay-start pf--dir-overlay)))
    ""))

(defun pf-propertize-filter-text ()
  "Format the filter text with prompt and faces."
  (set-text-properties (point-min) (overlay-start pf--dir-overlay)
                       pf--filter-properties))

(defun pf-filter-results ()
  "Filter the results using filter text."
  (pf--process-send (format "set %d %s\x00" (length pf--base-filter) pf--filter-text)))

(defun pf-self-filter-add ()
  "Add self to `project-find' search filter."
  (interactive)
  (let ((inhibit-read-only t)
        (c last-command-event))
    (when (characterp c)
      (pf-align-filter-input)
      (insert c))))

(defun pf-backward-delete-char (n)
  "Remove N chars from end of `project-find' search filter."
  (interactive "p")
  (if (< n 1)
      (error "ARG must be > 0"))
  (pf-align-filter-input)
  (let ((inhibit-read-only t))
    (delete-char (- n))))

(defun pf-root-dir (&optional dir buf)
  "Convert DIR to full-path relative to `project-root'.
If DIR is a full-path aleady return unchanged otherwise convert relative
to `project-root' (or `default-directory' if none) for BUF, defualt to
`current-buffer'."
  (if (or (string-prefix-p "~/" dir) (string-prefix-p "/" dir))
      dir
    (with-current-buffer (or buf (current-buffer))
      (concat
       (let ((pr (project-current nil)))
         (if pr (project-root pr) default-directory))
       dir))))

(defun pf-walk (dir &optional buf)
  "Start file search from DIR relative to `pf-root-dir'.
BUF is passed to `pf-root-dir' for relative paths."
  (setq dir (pf-root-dir dir buf))
  (with-current-buffer (pf-get-buffer-create)
    (setq default-directory dir)
    (let ((current-count pf--update-count))
      (pf--process-send (format "walk %s\x00" default-directory))
      (while (= current-count pf--update-count)
        (sit-for 0.016)))))

(defun pf-window-size (size)
  "Set the window SIZE of the servier process."
  (pf--process-send (format "window_size %s\x00" size)))

(defun pf-ignore (ignore)
  "Set the IGNORE pattern."
  (pf--process-send (format "ignore %s\x00" ignore)))

(defun pf-base-filter (text)
  "Set the TEXT used as a base filter."
  (setq pf--base-filter (concat (string-trim text) " "))
  (pf--process-send (format "set 0 %s %s\x00" pf--base-filter pf--filter-text)))

(defun pf-quit ()
  "Quit project find."
  (interactive)
  (switch-to-prev-buffer nil 'kill)
  (bury-buffer-internal (pf-get-buffer-create)))

(defun pf-get-buffer-create (&optional dir)
  "Get or create pf-buffer setting `default-directory' to DIR."
  (let* ((buf (get-buffer-create pf-buffer-name)))
    (with-current-buffer buf
      (when dir (setq default-directory dir))
      (unless (eq major-mode #'project-find-mode)
        (project-find-mode)
        (setq buffer-undo-list t)
        (cursor-intangible-mode 1)))
    buf))

(defun pf-kill-buffer ()
  "Kill the project-find buffer."
  (when (get-buffer pf-buffer-name)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*project-find*"))))

(defun pf-init (&optional name)
  "Initialize and switch to the project-find buffer.
Set the NAME for the type of find initialized."
  (let ((cdir default-directory))
    (pf-kill-buffer)
    (switch-to-buffer (pf-get-buffer-create cdir) t t)
    (setq-local left-margin-width 2)
    (set-window-buffer nil (pf-get-buffer-create))
    (if name
        (if (get-text-property 0 'face name)
            (setq pf--name name)
          (setq pf--name (propertize name 'face 'pf-name-face)))
      (kill-local-variable 'pf--name))
    (setq pf-find-function nil
          pf--base-filter ""
          pf-filter-changed-function nil
          pf--filter-text "")
    (use-local-map project-find-mode-map)))

(defun pf (&optional dir &rest args)
  "Do incremental search using `project-find' from `project-root'.
Start pf for the current buffer' `project-root'.  If DIR is not nil
start searching from DIR, which can be relative to `project-root'.
ARGS is keyword/argument pairs defined as follows:

:name NAME -- Title to show in the buffer before DIR.

:ignore IGNORE -- Pattern used to exclude files from results.

:base-filter BASE_FILTER -- Pattern to require for file to be in
results."
  (interactive)
  (let ((ignore (plist-get args :ignore))
        (base-filter (plist-get args :base-filter)))
    (pf-init (plist-get args :name))
    (pf--make-process)
    (when ignore (pf-ignore ignore))
    (when base-filter (pf-base-filter base-filter))
    (pf-window-size (- (window-text-height) 2))
    (pf-clear-output)
    (pf-walk (or dir ""))))

(defun pf--make-process ()
  "Start the koru_find program."
  (make-process :name "koru_find"
                :command (list pf-command "--server")
                :coding 'no-conversion
                :noquery t
                :buffer (current-buffer)
                :stderr "*koru_find::stderr*"
                :connection-type 'pipe
                :filter #'pf-process-filter
                :sentinel #'pf-process-sentinel))

(defun pf-clear-output ()
  "Clear buffer output and redraw filter string."
  (with-current-buffer (pf-get-buffer-create)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t))

      (erase-buffer)
      (remove-overlays)
      (setq pf--selected-lineno 0)
      (goto-char (point-min))
      (insert pf--filter-text "\n")
      (let ((start (point)))
        (insert pf--name
                (propertize default-directory 'face 'pf-dir-face)
                "\n")
        (move-overlay pf--dir-overlay start (point))
        (pf-propertize-filter-text)
        (goto-char (point-min))
        nil))))

(defun pf-backward-line (&optional n)
  "Select Nth line from current."
  (interactive "p")
  (pf-forward-line (- (or n 1))))

(defun pf-goto-results ()
  "Goto first char of results."
  (goto-char (overlay-end pf--dir-overlay)))

(defun pf--recalc-selected ()
  "Recalulate selected position.
Returns the start of the selection."
  (save-excursion
    (pf-goto-results)
    (setq pf--selected-lineno (or pf--selected-lineno 0))
    (let ((start (point))
          (count 0))
      (forward-line 1)
      (while (and (not (eobp)) (< count pf--selected-lineno))
        (setq start (point))
        (forward-line 1))

      (if (= start (point))
          (delete-overlay pf--selected-overlay)
        (setq pf--selected-lineno count)
        (move-overlay pf--selected-overlay start (point))
        start))))

(defun pf-forward-line (&optional n)
  "Select Nth line from current."
  (interactive "p")
  (save-excursion
    (setq n (or n 1))
    (let ((start (pf-selected-start)))
      (unless start
        (setq start (pf--recalc-selected)))
      (when start
        (goto-char start)
        (forward-line n)

        (let ((p (point)))
          (when (>= p (overlay-start (aref pf--margin-overlays 0)))
            (forward-line 1)
            (when (> (point) p)
              (setq pf--selected-lineno (+ pf--selected-lineno n))
              (move-overlay pf--selected-overlay p (point)))))))))

(defun pf-build-regex (text)
  "Build `pf-filter-re' from TEXT."
  (let ((esc nil))
    (setq pf-filter-re
          (mapconcat
           (lambda (c)
             (if (or esc (not (eq c ?\\)))
                 (progn
                   (when esc
                     (setq esc nil)
                     (when (eq c ?s)
                       (setq c ?\s)))
                   (if (eq c ?/)
                       "/.*"
                     (concat (regexp-quote (char-to-string c)) "[^/]*")))
               (setq esc t)
               ""
               ))
           text))))


(defun pf-find-project-open-project (start end)
  "Find project listed in `project-find' buffer between START and END.
This function is used to override `pf-find-function' and assumes that it
is always called from the `project-find' buffer."
  (let  ((path (buffer-substring-no-properties start end))
         (cmd (seq-find (lambda (elt) (functionp elt)) pf-find-project-open-commands)))
    (pf-quit)
    (funcall cmd  path)))

(defun pf-find-project-filter-changed (text _original)
  "Update `pf-find-project' results with new filter TEXT."
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (pf-build-regex text)
    (pf-find-project-list-projects)))

(defun pf-find-project-list-projects ()
  "List projects matching TEXT filter using `pf-find-project'."
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (save-excursion
      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t))
        (pf-goto-results)
        (delete-region (point) (point-max))
        (mapc (lambda (path)
                (when (string-match-p pf-filter-re path)
                  (pf-add-line path)))
              (project-known-project-roots))
        (pf-goto-results)
        (pf-post-process-filter (point) 0)))))

(defun pf-find-project ()
  "List all projects."
  (interactive)
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t)
        (dir (pf-root-dir)))
    (pf-init)
    ;; fixme! (use-local-map pf-find-project-local-map)

    (setq default-directory dir
          pf--name (propertize "Find project: " 'face 'pf-name-face)
          pf-filter-re ""
          pf-find-function #'pf-find-project-open-project
          pf-filter-changed-function #'pf-find-project-filter-changed)
    (pf-clear-output)
    (pf-find-project-list-projects)))


(provide 'project-find)

;;; project-find.el ends here
