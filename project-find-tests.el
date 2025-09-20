;;; project-find-tests.el --- ERT tests for project-find.el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'compile)
(require 'project-find)
(require 'project)

(defun log-msg (format-string &rest args)
  "Log a debug message.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((inhibit-message nil))
    (message "DEBUG: %s" (format format-string args))))

(defmacro pf-my-test-fixture (&rest body)
  "A fixture to set up a common environment for tests.  BODY is the test code."
  `(let ((inhibit-message t)
         (pf--update-count 0)
         (pf-command (concat (file-name-directory
                              (find-lisp-object-file-name #'pf nil))
                             "/koru_find")))
     (unwind-protect
         (progn ,@body)

       (pf-kill-process)
       (when (get-buffer "*project-find")
         (let ((kill-buffer-query-functions nil))
           (kill-buffer "*project-find*"))))))

(defun pf-my-add-keys (keys &optional ready-test time)
  "Add KEYS via `pf-self-filter-add'.
Call `pf--wait-for' with TIME and READY-TEST if not nil."
  (mapc (lambda (k)
          (let ((last-command-event k))(pf-self-filter-add)))
        keys)
  (when ready-test
    (pf--wait-for ready-test time)))

(ert-deftest pf-get-process ()
  (pf-my-test-fixture
   (let ((proc (pf-get-process)))
     (should proc)
     (should (eq proc (pf-get-process)))
     (pf-kill-process)
     (should (not (eq proc (pf-get-process)))))))


(ert-deftest pf-process-filter ()
  (pf-my-test-fixture
   (let* (lines
          (pline (lambda (x)
                   (setq lines (cons x lines))
                   nil))
          (pf--selected-overlay (make-overlay 1 1)))
     (unwind-protect
         (progn
           (advice-add #'pf-process-line :override pline)
           (pf-process-filter (pf-get-process) "+testing")
           (pf-process-filter (pf-get-process) "/123\x00+testing/45")
           (pf-process-filter (pf-get-process) "\x00")
           (pf-process-filter (pf-get-process) "-testz/13\x00")
           (pf-process-filter (pf-get-process) "-testz/56\x00")
           (should (equal (cadddr lines) "+testing/123"))
           (should (equal (caddr lines) "+testing/45"))
           (should (equal (cadr lines) "-testz/13"))
           (should (equal (car lines) "-testz/56")))
       (advice-remove #'pf-process-line pline)))))

(ert-deftest select-by-c-number ()
  (pf-my-test-fixture
   (switch-to-buffer (pf-get-buffer-create) t t)
   (let ((buffer-read-only nil)
         (inhibit-modification-hooks t))
     (pf-clear-output)
     (pf-add-line "test/1/2.txt")
     (pf-add-line "test/1/3.txt")
     (let ((last-command-event (aref (kbd "M-2") 0)))
       (pf-find-file-for-entry))
     (should (equal (buffer-name) "3.txt")))))



(ert-deftest pf-select-disapearing ()
  (pf-my-test-fixture
   (pf)

   (save-excursion
     (pf--wait-for
      (lambda ()
        (goto-char (point-min))
        (should (search-forward "Makefile")))))

   (pf-my-add-keys "M")

   (save-excursion
     (pf--wait-for
      (lambda ()
        (goto-char (point-min))
        (not (search-forward ".txt" nil t)))))

   (should (eq (current-buffer) (overlay-buffer pf--selected-overlay)))))

(ert-deftest pf--filter-properties ()
  (should (eq t (plist-get pf--filter-properties 'inhibit-read-only)))
  (should (eq 'pf-filter-face (plist-get pf--filter-properties 'face)))
  (should (equal (propertize "Filter: " 'face 'pf-filter-prompt-face)
                 (plist-get pf--filter-properties 'line-prefix))))


(ert-deftest pf ()
  (pf-my-test-fixture
   (pf)
   (should (eq buffer-read-only t))
   (should (eq (current-buffer) (get-buffer pf-buffer-name)))
   (should (eq (get-buffer-process (current-buffer)) (pf-get-process)))
   (pf-cd "test")

   (goto-char (point-min))

   (pf-my-add-keys "1")
   (should (= (point) (1- (overlay-start pf--dir-overlay))))

   (goto-char (point-min))

   (should (looking-at "1"))
   (should (equal (text-properties-at (point))
                  pf--filter-properties))

   (save-excursion
     (pf--wait-for
      (lambda ()
        (goto-char (point-min))
        (should (search-forward "1/2.txt"))
        (forward-line 1)
        (should (when-let* ((ol (car (overlays-at (point))))
                            (p (overlay-get ol 'face)));
                  (eq p 'pf-common-prefix-face)))
        (should (when-let* ((ol (car (overlays-in (point) (point))))
                            (p (overlay-get ol 'before-string))
                            (m (get-text-property 0 'display p)))
                  (should (equal m '((margin left-margin) "2")))
                  (equal p " ")))
        (should (search-forward "1/3.txt")))))
   (let ((count (1+ pf--update-count)))
     (pf-cd "test/1")
     (pf--wait-for (lambda ()
                     (>= pf--update-count count))))


   (goto-char (point-max))
   (pf-my-add-keys "2")
   (should (= (point) (1- (overlay-start pf--dir-overlay))))

   (pf)
   (pf-cd "test/1")
   (pf-my-add-keys "2")

   (save-excursion (pf--wait-for (lambda ()
                                   (goto-char (point-min))
                                   ( should (search-forward "2.txt")))))

   (pf-backward-delete-char 1)

   (pf-my-add-keys "3")
   (pf-my-add-keys "t")

   (save-excursion
     (should (pf--wait-for (lambda ()
                             (goto-char (point-min))
                             (search-forward "3.txt" nil t)
                             (forward-line 0)
                             (looking-at "3.txt")))))))

(ert-deftest pf-with-ignore ()
  (pf-my-test-fixture
   (pf ">3.txt" "test")
   (pf-my-add-keys "1")

   (save-excursion
     (pf--wait-for (lambda ()
                     (pf-goto-results)
                     (should (search-forward "1/2.txt" nil t))
                     (forward-line 1)
                     (should (not (search-forward "1/3.txt" nil t))))))

   (pf-cd "test/1")
   (save-excursion
     (pf--wait-for (lambda ()
                     (pf-goto-results)
                     (should (search-forward "2.txt"))
                     (should (not (search-forward "3.txt" nil t))))))))

(ert-deftest pf-process-line ()
  (pf-my-test-fixture
   (with-current-buffer (pf-get-buffer-create)
     (let ((buffer-read-only nil)
           (inhibit-modification-hooks t) )
       (pf-clear-output)
       (pf-process-line "+a")
       (pf-process-line "+b")
       (pf-process-line "+ab")
       (pf-goto-results)
       (should (looking-at "a\nab\nb"))
       (pf-process-line "-ab")
       (should (looking-at "a\nb"))
       (pf-process-line "+c")
       (pf-process-line "+d")
       (pf-process-line "+e")
       (pf-process-line "-a")
       (pf-process-line "-b")
       (pf-process-line "-e")
       (should (looking-at "c\nd\n"))))))

(ert-deftest pf--find-common-dir ()
  (should (eq (pf--find-common-dir "ab/d" "ab/c") 3))
  (should (eq (pf--find-common-dir "/d" "/c") 1))
  (should (not (pf--find-common-dir "abd" "abc"))))

(ert-deftest pf-find-selected ()
  (pf-my-test-fixture
   (pf)
   (pf-my-add-keys ">.txt")
   (should (pf--wait-for
            (lambda ()
              (pf-goto-results)
              (= 2 (count-lines (point) (point-max))))))
   (pf-forward-line)
   (save-excursion
     (goto-char (overlay-start pf--selected-overlay))
     (should (looking-at "test/1/3.txt")))
   (pf-find-selected)
   (should (equal (buffer-name) "3.txt"))))

(ert-deftest pf-forward-line ()
  (pf-my-test-fixture
   (pf)
   (should (pf--wait-for (lambda () (overlay-buffer pf--selected-overlay))))
   (save-excursion
     (pf-goto-results)
     (should (= (point) (overlay-start pf--selected-overlay)))
     (pf-forward-line)
     (forward-line 1)
     (should (= (point) (overlay-start pf--selected-overlay))))
   (should
    (pf-my-add-keys
     "Make" (lambda () (not (overlay-buffer (aref pf--margin-overlays 1))))))

   (pf-backward-line)
   (should (overlay-buffer pf--selected-overlay))

   (should
    (pf-my-add-keys
     "z" (lambda () (not (overlay-buffer pf--selected-overlay)))))

   (pf-forward-line)
   (should
    (not (overlay-buffer pf--selected-overlay)))))

;;; project-find-tests.el ends here
