;;; init-git.el --- set up git configuration
;;; Commentary:
;;; Code:

(require-package 'magit)
(require 'magit)

(global-set-key (kbd "\C-c g") 'magit-status)

;; Disable linum mode
(add-hook 'magit-mode-hook
          (lambda ()
              (linum-mode -1)))
(setq magit-last-seen-setup-instructions "1.4.0")
(provide 'init-git)
;;; init-git.el ends here
