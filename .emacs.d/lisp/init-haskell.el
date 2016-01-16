;;; init-haskell.el
;;; Commentary:
;;; Code:
(require-package 'haskell-mode)
(require 'haskell-mode)
(require 'smartparens-haskell)
(sp-with-modes 'haskell-mode
  (sp-local-pair "'" nil :actions nil))



(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


(let ((my-cabal-path (expand-file-name "~/Library/Haskell/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(require-package 'flycheck-haskell)

(setq
 haskell-notify-p t
 haskell-tags-on-save t
 haskell-interactive-popup-error nil
 haskell-process-suggest-remove-import-lines t
 haskell-process-auto-import-loaded-modules t
 haskell-stylish-on-save nil)

(require-package 'ghc)
;;(require 'ghc)
;;(add-hook 'haskell-mode-hook 'ghc-init)
;; (set-face-attribute 'ghc-face-error nil :underline nil)
;; (set-face-attribute 'ghc-face-warn nil :underline nil)

;;(require-package 'company-ghc)
;;(require 'company)
;; (require-package 'company-cabal)

;;(add-to-list 'company-backends 'company-ghc)
;;(custom-set-variables '(company-ghc-show-info t))
;;(add-to-list 'company-backends 'company-ghc)
;;(add-to-list 'company-backends 'company-backends-haskell)
;; (add-to-list 'company-backends 'company-cabal)
;; (push '(company-ghc company-dabbrev-code company-yasnippet)
;;       company-backends-haskell-mode)

;;(custom-set-variables '(company-ghc-show-info t))
;; (eval-after-load 'company
;; 	    '(add-to-list 'company-backends
;; 			  '(company-ghc
;; 			    company-cabal
;; 			    company-backends-haskell)))

;;; init-haskell.el ends here
(provide 'init-haskell)

