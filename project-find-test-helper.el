;;; project-find-test-helper.el --- Helper functions for tests  -*- lexical-binding: t; -*-
;;; Commentary:
;; Use these functions to help write tests that use project-find.
;;; Code:

(require 'project-find)

(defun log-msg (format-string &rest args)
  "Log a debug message.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((inhibit-message nil))
    (message "DEBUG: %s" (format format-string args))))

(defmacro pf-my-test-fixture (&rest body)
  "A fixture to set up a common environment for tests.  BODY is the test code."
  `(let ((inhibit-message t)
         (pf--update-count 0)
         (pf-filter-re "")
         (my-pf-kill-buffers '("2.txt" "3.txt"))
         (pf-command (concat (file-name-directory
                              (find-lisp-object-file-name #'pf nil))
                             "/koru_find")))
     (unwind-protect
         (progn ,@body)
       (let ((kill-buffer-query-functions nil))
         (mapc (lambda (bn)
                 (when (get-buffer bn) (kill-buffer bn)))
               my-pf-kill-buffers))
       (pf-kill-buffer))))

(defun pf-my-add-keys (keys &optional ready-test time)
  "Add KEYS via `pf-self-filter-add'.
Call `pf--wait-for' with TIME and READY-TEST if not nil."
  (mapc (lambda (k)
          (let ((last-command-event k))(pf-self-filter-add)))
        keys)
  (when ready-test
    (pf--wait-for ready-test time)))

(provide 'project-find-test-helper)
;;; project-find-test-helper.el ends here
