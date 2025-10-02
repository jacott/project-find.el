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

(defvar pf-process nil
  "Process for finding files and content.")

(defvar pf-buffer-name "*project-find*"
  "Name of buffer for finding.")

(defvar pf-process-output ""
  "Buffer for process output.")

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

(defface pf-filter-prompt-face '((t :inherit font-lock-keyword-face))
  "Face for filter text."
  :group 'pf-faces)

(defconst pf--filter-properties
  (list
   'inhibit-read-only t
   'face 'pf-filter-face
   'line-prefix (propertize "Filter: " 'face 'pf-filter-prompt-face))
  "The prompt displayed before filter text.")

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
  (overlay-put pf--dir-overlay 'face 'pf-dir-face))

(defun pf-buffer-changed (_start _end _old-len)
  "Hook that sends any changes of the filter to the back-end server."
  (let ((text (pf-filter-text))
        (original pf--filter-text))
    (when (not (equal text pf--filter-text))
      (setq pf--filter-text text)
      (pf-propertize-filter-text)
      (if pf-filter-changed-function
          (funcall pf-filter-changed-function text original)
        (process-send-string (pf-get-process) (format "set 0 %s\x00" text))))))

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
      (pf-goto-results)
      (let* ((lineno 0)
             (start (point))
             (end (progn (forward-line 1) (point))))
        (while (and
                (< start end)
                (string> line (buffer-substring-no-properties start (1- end))))
          (setq lineno (1+ lineno))
          (setq start end)
          (setq end (progn (forward-line 1) (point ))))
        (goto-char start)
        (insert line "\n")
        (pf--set-common-prefix)
        (when (not (eobp))
          (forward-line 1)
          (pf--set-common-prefix))
        (cons start lineno)))))

(defun pf-rm-line (line)
  "Remove LINE from buffer.  Return pos and lineno of removal."
  (with-current-buffer (pf-get-buffer-create)
    (save-excursion
      (pf-goto-results)
      (let ((start (point))
            (end (progn (forward-line 1) (point)))
            (lineno 0))
        (while (and
                (< start end)
                (string> line (buffer-substring start (1- end))))
          (setq lineno (1+ lineno))
          (setq start end)
          (setq end (progn (forward-line 1) (point))))
        (when (string= line (buffer-substring-no-properties start (1- end)))
          (delete-region start end)
          (goto-char start)
          (when (not (eobp))
            (forward-line 1)
            (pf--set-common-prefix)))
        (cons start lineno)))))

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
  (message "pf-process %s" event)
  (setq pf-process nil))

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
  (process-send-string (pf-get-process) (format "set 0 %s\x00" pf--filter-text)))

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

(defun pf-cd (dir)
  "Change search to DIR."
  (with-current-buffer (pf-get-buffer-create)
    (setq default-directory dir)
    (let ((current-count pf--update-count))
      (process-send-string (pf-get-process) (format "cd %s\x00" dir))
      (while (= current-count pf--update-count)
        (sit-for 0.016)))))

(defun pf-window-size (size)
  "Set the window SIZE of the servier process."
  (process-send-string (pf-get-process) (format "window_size %s\x00" size)))

(defun pf-ignore (ignore)
  "Set the IGNORE pattern."
  (process-send-string (pf-get-process) (format "ignore %s\x00" ignore)))

(defun pf-quit ()
  "Quit project find."
  (interactive)
  (switch-to-prev-buffer nil 'kill)
  (bury-buffer-internal (pf-get-buffer-create)))

(defun pf-get-buffer-create ()
  "Get or create pf-buffer."
  (let* ((pr (project-current nil))
         (root (if pr (project-root pr) default-directory))
         (buf (get-buffer-create pf-buffer-name)))
    (with-current-buffer buf
      (setq default-directory root)
      (unless (eq major-mode #'project-find-mode)
        (project-find-mode)))
    buf))

(defun pf-get-process ()
  "Get or start the koru_find program."
  (if (process-live-p pf-process)
      pf-process
    (setq pf-process (make-process :name "koru_find"
                                   :command (list pf-command "--server")
                                   :coding 'no-conversion
                                   :noquery t
                                   :buffer (pf-get-buffer-create)
                                   :stderr "*koru_find::stderr*"
                                   :connection-type 'pipe
                                   :filter #'pf-process-filter
                                   :sentinel #'pf-process-sentinel))))
(defun pf-kill-process ()
  "Kill any running koru_find program."
  (when (process-live-p pf-process)
    (delete-process pf-process)))

(defun pf-init ()
  "Initialize and switch to the project-find buffer."
  (when-let* ((cwin (get-buffer-window))
              (win (get-buffer-window pf-buffer-name t)))
    (select-window win t)
    (switch-to-prev-buffer nil 'kill)
    (bury-buffer-internal (pf-get-buffer-create))
    (select-window cwin t))
  (switch-to-buffer (pf-get-buffer-create) t t)
  (setq-local left-margin-width 2)
  (setq pf-find-function nil
        pf-filter-changed-function nil
        pf--filter-text "")
  (use-local-map project-find-mode-map)
  (set-window-buffer nil (pf-get-buffer-create))
  (pf-window-size (- (window-text-height) 2)))

(defun pf (&optional ignore dir)
  "Do incremental search using `project-find'.
Start pf with an IGNORE pattern.  If DIR is nil use `default-directory'."
  (interactive)
  (pf-get-process)
  (pf-init)

  (when ignore (pf-ignore ignore))
  (pf-cd (or dir default-directory)))

(defun pf-clear-output ()
  "Clear buffer output and redraw filter string."
  (with-current-buffer (pf-get-buffer-create)
    (erase-buffer)
    (remove-overlays)
    (setq pf--selected-lineno 0)
    (goto-char (point-min))
    (insert pf--filter-text "\n")
    (let ((start (point)))
      (insert (format "%s\n" default-directory))
      (move-overlay pf--dir-overlay start (point))
      (pf-propertize-filter-text)
      (goto-char (point-min))
      nil)))

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

(provide 'project-find)

;;; project-find.el ends here
