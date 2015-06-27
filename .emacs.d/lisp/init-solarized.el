;;; init-solarized.el --- solarized theme
;;; Commentary:
;; Load solarized theme
;;; Code:

(require-package 'color-theme-solarized)
(require 'cl)
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)
(set-terminal-parameter nil 'background-mode 'dark)

(provide 'init-solarized)
;;; init-solarized ends here
