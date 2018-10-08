;;; package --- summary:
;;; Commentary:
;;; Code:
(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package better-defaults
  :ensure t
  :config
  (ido-mode -1)
  (setq visible-bell nil))
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package diminish
  :ensure t)

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :config
  (which-key-mode))

(use-package smex
  :ensure t)

;; TODO Make swiper work more like isearch (wraparound)
(use-package counsel
  :ensure t
  :diminish ivy-mode
  :after smex
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . counsel-grep-or-swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq counsel-grep-base-command
	"rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package avy
  :ensure t
  :bind  ("C-'" . avy-goto-char-2))

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :config
  ;; This doesn't appear to work properly.
  (setq projectile-project-search-path '("~/")))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :bind ("C-c p" . projectile-command-map)
  :config
  (counsel-projectile-mode 1))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package company
  :diminish company-mode
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-complete-number t))

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :config
  (global-flycheck-mode))

(use-package evil
  :ensure t
  :diminish undo-tree-mode)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode)))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package sqlup-mode
  :ensure t
  :hook (sql-mode . sqlup-mode))

(use-package sql-indent
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode "\\.\\(yml\\|yaml\\)$")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package terraform-mode
  :ensure t
  :mode "\\.tf")

(use-package org
  :ensure t
  :config
  (setq org-use-speed-commands 1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package markdown-mode
  :ensure t
  :mode "\\.md")

(provide 'init)
;;; init.el ends here
