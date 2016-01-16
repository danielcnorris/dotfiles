;;; init-helm.el
;;; Commentary:
;;; Code:
(require-package 'helm)
(require-package 'helm-descbinds)
(require-package 'helm-swoop)

(global-set-key (kbd "C-;") 'avy-goto-char)

(require 'helm-config)
(require 'recentf)
(helm-descbinds-mode)
(helm-mode 1)
(recentf-mode)

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-command-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-command-map (kbd "C-z")  'helm-select-action)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

(require 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-r") 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-multi-swoop)
(global-set-key (kbd "M-I") 'helm-multi-swoop-all)

(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Don't prepopulate
(setq helm-swoop-pre-input-function
             (lambda () nil))

(setq helm-multi-swoop-edit-save t)
(setq helm-swoop-split-with-multiple-windows nil)

;; Swoop solarized
;; Will look bad in terminal but good in GUI
(set-face-attribute 'helm-swoop-target-word-face nil
		    :background "#b58900"
		    :foreground "#002b36")

(set-face-attribute 'helm-swoop-target-line-face nil
		    :background "#cb4b16"
		    :foreground "#002b36")

(set-face-attribute 'helm-swoop-target-line-block-face nil
		    :background "#cb4b16"
		    :foreground "#002b36")


;;; init-helm ends here
(provide 'init-helm)
