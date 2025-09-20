;;; init.el --- Init file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Setup for testing.
;;
;;; Code:

(require 'package)

(setq package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))

(package-initialize)


;;; init.el ends here
