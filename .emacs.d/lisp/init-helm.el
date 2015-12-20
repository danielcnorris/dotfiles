;;; init-helm.el
;;; Commentary:
;;; Code:
(require-package 'helm)
(require-package 'helm-swoop)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-command-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-command-map (kbd "C-z")  'helm-select-action)

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h r") 'helm-info-emacs)

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-r") 'helm-swoop)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; Swoop solarized
;; Will look bad in terminal but good in GUI
(eval-after-load 'helm-swoop
  '(progn
     (set-face-attribute 'helm-swoop-target-word-face nil
			 :background "#b58900"
			 :foreground "#002b36")

     (set-face-attribute 'helm-swoop-target-line-face nil
			 :background "#cb4b16"
			 :foreground "#002b36")

     (set-face-attribute 'helm-swoop-target-line-block-face nil
			 :background "#cb4b16"
			 :foreground "#002b36")))


(helm-mode 1)

(require-package 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char)
;;; init-helm ends here
(provide 'init-helm)
