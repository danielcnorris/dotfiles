;;; init-solarized.el --- solarized theme
;;; Commentary:
;; Load solarized theme
;;; Code:

(require-package 'color-theme-solarized)
(require 'cl)
(setq frame-background-mode 'dark)
(load-theme 'solarized t)

(provide 'init-solarized)
;;; init-solarized ends here
