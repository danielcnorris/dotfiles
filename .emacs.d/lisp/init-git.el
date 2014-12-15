;;; init-git.el --- set up git configuration
;;; Commentary:
;;; Code:

(require-package 'magit)
(require 'magit)

(global-set-key (kbd "\C-c g") 'magit-status)

(provide 'init-git)
;;; init-git.el ends here
