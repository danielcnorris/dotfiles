;;; init.el --- main config init file
;;; Commentary:
;; Daniel Norris's Emacs configuration
;;; Code:

;; Load secrets
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'secrets)

(require 'init-utils)
(require 'init-elpa)
(require 'init-solarized)
(require 'init-defaults)
;;(require 'init-swiper)
(require 'init-helm)
(require 'init-org)
;; (require 'init-ido)
;; (require 'init-dired)
;; (require 'init-auto-complete)
;; (require 'init-bbdb)
;; (require 'init-flyspell)
;; (require 'init-flycheck)
;; (require 'init-git)
;; (require 'init-pdf)
;; (require 'init-chromium)
;; ;;(require 'init-haskell)
;; (require 'init-html)
;; ;;(require 'init-javascript)
;; (require 'init-lisp)
;; (require 'init-term)
;; (require 'init-ledger)
;; (require 'init-ess)

;; Open org on load
;;(find-file (concat dcn/org-directory "todo.org"))

;; Set default dired directory back to home
;;(setq default-directory dcn/home-directory)

(provide 'init)
;;; init.el ends here
