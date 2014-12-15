;;; init-term.el --- Terminal Emulator support
;;; Commentary:
;;; Code:

(require-package 'multi-term)

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(global-set-key (kbd "\C-cd") 'multi-term)

(provide 'init-term)
;;; init-term.el ends here
