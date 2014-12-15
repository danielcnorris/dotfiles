;;; init-dired.el --- Dired settings
;;; Commentary:
;;; Code:

(require-package 'dired+)
(require-package 'dired-sort)

(after-load 'dired
            (require 'dired+)
            (require 'dired-sort))

(global-set-key (kbd "\C-xj") 'dired-jump)

(provide 'init-dired)
;;; init-dired.el ends here
