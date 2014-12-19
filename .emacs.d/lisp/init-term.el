;;; init-term.el --- Terminal Emulator support
;;; Commentary:
;;; Code:

(require-package 'multi-term)

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(global-set-key (kbd "\C-cd") 'multi-term)

(add-hook 'term-mode-hook
          (lambda ()
              ;; Allow navigation between terminal buffers
              (add-to-list 'term-bind-key-alist '("M-["
                                                  . multi-term-prev))
              (add-to-list 'term-bind-key-alist '("M-]"
                                                  . multi-term-next))
              ;; Disable linum mode
              (linum-mode -1)

              ;; Enable proper pasting
              (define-key term-raw-map (kbd "\C-y") 'term-paste)))

(provide 'init-term)
;;; init-term.el ends here
