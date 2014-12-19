;;; init-git.el --- set up git configuration
;;; Commentary:
;;; Code:

(require-package 'magit)
(require 'magit)

(global-set-key (kbd "\C-c g") 'magit-status)

(add-hook 'magit-mode-hook
          (lambda ()
              (linum-mode -1)))

(provide 'init-git)
;;; init-git.el ends here
