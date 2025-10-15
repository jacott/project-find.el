;; project-find-tests.el --- ERT tests for project-find  -*- lexical-binding: t; -*-
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
;;
;;; Commentary:
;; ERT tests for project-find
;;
;;; Code:

(require 'project-find)
(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'compile)
(require 'project)
(require 'project-find-test-helper)

(defmacro pf-my-test-send (&rest body)
  "A fixture to set up a common environment for tests.  BODY is the test code."
  `(pf-my-test-fixture
    (let* (results
           (override (lambda (x)
                       (setq results (cons x results))
                       nil)))
      (unwind-protect
        (progn
          (advice-add #'pf--process-send :override override)
          ,@body)
      (advice-remove #'pf--process-send override)))))

(ert-deftest pf-tab-menu ()
  (pf-my-test-fixture
   (should (equal (cadr pf-option-menu-map) '(t . pf-option-menu)))
   (pf)
   (should (eq (keymap-local-lookup "TAB") #'pf-option-menu))
   (pf-option-menu)
   (should (equal (overlay-start pf--option-menu-help-overlay) 1))
   (should (equal (overlay-end pf--option-menu-help-overlay) 66))
   (goto-char (point-min))
   (should (looking-at "Filter options:"))
   (should (eq (keymap-local-lookup "b") #'pf-toggle-matching-buffer))
   (pf-option-menu-off)
   (should (equal (overlay-start pf--option-menu-help-overlay) 1))
   (should (equal (overlay-end pf--option-menu-help-overlay) 1))
   (should (eq (keymap-local-lookup "b") nil))
   ))

(ert-deftest pf-limit-to-buffers ()
  (pf-my-test-fixture
   (find-file-noselect "test/1/2.txt")
   (find-file-noselect "test/1/3.txt")
   (pf)
   (should (pf--wait-for (lambda ()
                           (pf-goto-results)
                           (search-forward "test/a/1.json" nil t)
                           )))
   (should (not pf-matching-buffer))
   (pf-toggle-matching-buffer)
   (should pf-matching-buffer)
   (should (pf--wait-for (lambda ()
                           (pf-goto-results)
                           (not (search-forward "test/a/1.json" nil t))
                           )))
   (should (pf--wait-for (lambda ()
                           (pf-goto-results)
                           (and (search-forward "test/1/2.txt" nil t)
                                (search-forward "test/1/3.txt" nil t))
                           )))

   (pf-my-add-keys "2")
   (should (pf--wait-for (lambda ()
                           (pf-goto-results)
                           (not (search-forward "3.txt" nil t))
                           )))
   (pf-backward-delete-char 1)
   (should (pf--wait-for (lambda ()
                           (pf-goto-results)
                           (and (search-forward "test/1/2.txt" nil t)
                                (search-forward "test/1/3.txt" nil t))
                           )))

   (pf-toggle-matching-buffer)
   (should (not pf-resync-function))
   (should (pf--wait-for (lambda ()
                           (pf-goto-results)
                           (search-forward "test/a/1.json" nil t)
                           )))
   ))

(ert-deftest pf ()
  (pf-my-test-fixture
   (pf)
   (should (eq buffer-read-only t))
   (should (eq (current-buffer) (get-buffer pf-buffer-name)))
   (pf-walk "test")

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
     (pf-walk "test/1")
     (pf--wait-for (lambda ()
                     (>= pf--update-count count))))


   (goto-char (point-max))
   (pf-my-add-keys "2")
   (should (= (point) (1- (overlay-start pf--dir-overlay))))

   (pf "test/1")
   (pf-my-add-keys "2")

   (save-excursion
     (should (pf--wait-for (lambda ()
                             (goto-char (point-min))
                             (search-forward "2.txt")))))

   (pf-backward-delete-char 1)
   (pf-my-add-keys "3")
   (pf-my-add-keys "t")

   (save-excursion
     (should (pf--wait-for (lambda ()
                             (goto-char (point-min))
                             (search-forward "3.txt" nil t)
                             (forward-line 0)
                             (looking-at "3.txt")))))))

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
           (pf-process-filter nil "+testing")
           (pf-process-filter nil "/123\x00+testing/45")
           (pf-process-filter nil "\x00")
           (pf-process-filter nil "-testz/13\x00")
           (pf-process-filter nil "-testz/56\x00")
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
       (pf-find-entry))
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

(ert-deftest pf-build-regex ()
  (pf-my-test-fixture
   (should (equal (pf-build-regex "abc") "a[^/]*b[^/]*c[^/]*"))
   (should (equal (pf-build-regex "a\\s[b]*c") "a[^/]* [^/]*\\[[^/]*b[^/]*][^/]*\\*[^/]*c[^/]*"))
   ))

(ert-deftest pf-find-project ()
  (pf-my-test-fixture
   (let ((proots (lambda ()
                   '("p/1/p3/" "test/1/" "p/1/p1/")))
         open-dir)

     (defun pf-my-open-project-command (dir)
       "Testing function."
       (setq open-dir dir))

     (defun pf-my-too-late-open-project-command (_dir)
       "Testing function."
       (setq open-dir nil))

     (unwind-protect
         (progn
           (advice-add #'project-known-project-roots :override proots)

           (pf-find-project)
           (setq-local pf-find-project-open-commands
                       '(pf-my-non-existant-function pf-my-open-project-command
                                                     pf-my-too-late-open-project-command dired))
           (pf-goto-results)
           (save-excursion
             (should
              (pf--wait-for (lambda ()
                              (pf-goto-results)
                              (search-forward "p/1/p1" nil t)))))

           (pf-my-add-keys "t/1")
           (pf-goto-results)

           (save-excursion
             (should
              (pf--wait-for (lambda ()
                              (pf-goto-results)
                              (not (search-forward "p/1" nil t))))))

           (pf-backward-delete-char 3)

           (save-excursion
             (should
              (pf--wait-for (lambda ()
                              (pf-goto-results)
                              (search-forward "p/1" nil t)))))

           (pf-forward-line 2)

           (pf-find-selected)
           (should (string-suffix-p "test/1" open-dir)))
       (advice-remove #'project-known-project-roots proots))))
  (should (not (get-buffer pf-buffer-name))))

(ert-deftest pf-find-project-with-limit ()
  (pf-my-test-fixture
   (let* ((len (length default-directory))
          (list '(("ptest/a/" . "test/a/")
                  ("ptest/b/" . "test/b/")
                  ("ptest/1/" . "test/1/")
                  ("ptest/1/a/" . "test/1/")))
          (proot (lambda (pr)
                   (alist-get pr list default-directory nil #'equal)))
          (pcur (lambda (&optional p dir)
                  (should (not p))
                  (concat "p" (substring (abbreviate-file-name (or dir default-directory)) len nil)))))

     (mapc (lambda (item)
             (find-file-noselect (substring (car item) 1 nil)))
           list)

     (unwind-protect
         (progn
           (advice-add #'project-current :override pcur)
           (advice-add #'project-root :override proot)

           (pf-find-project '(4))
           (should (equal 1 pf-limit-to-recent))
           (pf-goto-results)
           (save-excursion
             (should
              (pf--wait-for (lambda ()
                              (pf-goto-results)
                              (search-forward "test/1" nil t)))))

           (should
           (save-excursion
             (pf-goto-results)
             (not (search-forward "p/1/p1" nil t)))))

       (advice-remove #'project-root proot)
       (advice-remove #'project-current pcur)))))

(ert-deftest pf-with-base-filter ()
  (pf-my-test-fixture
   (pf "test" :name "my-find" :base-filter "  >2.txt")
   (pf-my-add-keys "1")
   (should (equal pf--base-filter ">2.txt "))

   (save-excursion
     (pf--wait-for (lambda ()
                     (pf-goto-results)
                     (should (search-forward "1/2.txt" nil t))
                     (forward-line 1)
                     (should (not (search-forward "1/3.txt" nil t))))))

   (pf "test/1")
   (should (equal pf--base-filter ""))
   (save-excursion
     (pf--wait-for (lambda ()
                     (pf-goto-results)
                     (should (search-forward "3.txt")))))))


(ert-deftest pf-with-ignore ()
  (pf-my-test-fixture
   (pf "test" :name "my-find" :ignore ">3.txt")
   (pf-my-add-keys "1")

   (save-excursion
     (pf--wait-for (lambda ()
                     (pf-goto-results)
                     (should (search-forward "1/2.txt" nil t))
                     (forward-line 1)
                     (should (not (search-forward "1/3.txt" nil t))))))

   (pf-walk "test/1")
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

(ert-deftest pf-overrides ()
  (pf-my-test-fixture
   (pf-init)
   (should (eq (current-local-map) project-find-mode-map))
   (use-local-map (make-sparse-keymap))
   (should (not (eq (current-local-map) project-find-mode-map)))
   (setq pf-find-function #'ignore
         pf-filter-changed-function #'ignore
         pf--filter-text "set")
   (pf) ;; calls pf-init
   (should (eq (current-local-map) project-find-mode-map))
   (should (eq pf-find-function nil))
   (should (eq pf-filter-changed-function nil))
   (should (equal pf--filter-text ""))

   (let* (text
          (pf-find-function (lambda (start end)
                              (setq text (buffer-substring-no-properties start end))
                              )))
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
     (should (equal text "test/1/3.txt")))))

(ert-deftest pf-skip-prefix ()
  (pf-my-test-send
   (pf-skip-prefix 6)
   (should (equal results '("skip-prefix 6\x00")))))

(ert-deftest pf-stop-matching ()
  (pf-my-test-send
   (pf-stop-matching)
   (should (equal results '("stop\x00")))))

(ert-deftest pf-resend-filter ()
  (pf-my-test-send
   (switch-to-buffer (pf-get-buffer-create) t t)
   (setq pf--base-filter "base:"
         pf--filter-text "ft")

   (pf-resend-filter)
   (should (equal results '("set 0 base:ft\x00")))))


(ert-deftest pf-custom-find ()
  (pf-my-test-fixture
   (let ((resync-func (lambda ()
                        (pf-match-line "2b-line1")
                        (pf-match-line "2b-notlineb")
                        (pf-match-line "1a-line2")))
         results)
     (pf-init "My Tests: ")

     (setq
      pf-find-function (lambda (start end)
                         (setq results (cons (list "pf-find-function" start end) results)))
      pf-propertize-line (lambda (line)
                           (put-text-property 0 1 'invisible t line)
                           line)
      pf-resync-function resync-func)
     (pf-clear-output)
     (pf-skip-prefix 3)
     (pf-my-add-keys "<line")

     (should (pf--wait-for
              (lambda ()
                (pf-goto-results)
                (and (not (search-forward "notlineb" nil t))
                     (progn
                       (pf-goto-results)
                       (search-forward "line1" nil t))))))
     (pf-find-selected)
     (should (equal results '(("pf-find-function" 43 51)))))))

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
