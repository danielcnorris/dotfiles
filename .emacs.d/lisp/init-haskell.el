;;; init-haskell.el --- haskell settings
;;; Commentary:
;;; Code:

(require-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'init-haskell)
;;; init-haskell.el ends here
