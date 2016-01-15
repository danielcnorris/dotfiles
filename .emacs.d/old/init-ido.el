;;; init-ido.el --- set up ido
;;; Commentary:
;; Configures Ido mode
;;; Code:

(require-package 'ido-hacks)
(require-package 'ido-vertical-mode)
(require-package 'smex)

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(require 'ido-hacks)
(ido-vertical-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'init-ido)
;;; init-ido.el ends here