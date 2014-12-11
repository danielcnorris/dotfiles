;;; init-auto-complete.el --- configure autocomplete
;;; Commentary:
;; Autocomplete mode setup
;;; Code:

(require-package 'auto-complete)
(require 'auto-complete)
(global-auto-complete-mode t)
(ac-linum-workaround)

(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'coffee-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)

(provide 'init-auto-complete)
;; init-auto-complete.el ends here
