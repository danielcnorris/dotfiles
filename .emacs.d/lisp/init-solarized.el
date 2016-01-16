;;; init-solarized.el --- solarized theme
;;; Commentary:
;; Load solarized theme
;;; Code:
(require 'cl)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(require-package 'color-theme-solarized)
(load-theme 'solarized t)

(provide 'init-solarized)
;;; init-solarized ends here
