;;; init-term.el --- Terminal Emulator support
;;; Commentary:
;;; Code:

(require-package 'multi-term)

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(global-set-key (kbd "\C-cd") 'multi-term)

;; Allow navigation between terminal buffers
(add-hook 'term-mode-hook
          (lambda ()
              (add-to-list 'term-bind-key-alist '("M-["
                                                  . multi-term-prev))
              (add-to-list 'term-bind-key-alist '("M-]"
                                                   . multi-term-next))))

;; Enable proper pasting into terminal buffers
(add-hook 'term-mode-hook
          (lambda ()
              (define-key term-raw-map (kbd "\C-y") 'term-paste)))

(provide 'init-term)
;;; init-term.el ends here
