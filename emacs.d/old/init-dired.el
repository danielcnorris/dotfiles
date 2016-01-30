;;; init-dired.el --- Dired settings
;;; Commentary:
;;; Code:

(require-package 'dired+)
(require-package 'dired-sort)

(after-load 'dired
            (require 'dired+)
            (require 'dired-sort))

(global-set-key (kbd "\C-xj") 'dired-jump)

;; Disable linum in org agenda
(add-hook 'dired-mode-hook
          (lambda ()
              (linum-mode -1)))

(provide 'init-dired)
;;; init-dired.el ends here
