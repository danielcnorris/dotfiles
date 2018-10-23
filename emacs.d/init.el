;;; package --- summary:
;;; Commentary:
;;; Code:

;; TODO Set up Org files (todo.org, journal.org, notes.org)
;; TODO Set the first buffer to show (?)
;; TODO Determine whether / when to enable evil by default.
;; TODO Use ensure-system-package for external executable dependencies.
;; TODO Email
;; TODO Pass integration
(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package auto-package-update
  :ensure t
  :defines (auto-package-update-delete-old-versions
            auto-package-update-hide-results)
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; https://emacs.stackexchange.com/questions/16818/cocoa-emacs-24-5-font-issues-inconsolata-dz/29397#29397
(set-frame-font "InconsolataG 13" nil t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package better-defaults
  :ensure t
  :diminish auto-revert-mode
  :config
  ;; TODO https://github.com/technomancy/better-defaults/pull/25
  (ido-mode -1)
  (setq visible-bell nil))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t))
      create-lockfiles nil)

(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "M-h") 'help-command)
(global-set-key (kbd "C-c C-h") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg))
  (indent-according-to-mode))
(define-key global-map (kbd "C-u") 'backward-kill-line)

;; TODO Get rid of free variable warning.
(setq inhibit-startup-message t
      initial-scratch-message ""
      ring-bell-function 'ignore
      tags-revert-without-query t
      tags-add-tables nil
      large-file-warning-threshold nil)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; https://emacs.stackexchange.com/questions/17005/killing-ansi-term-says-has-a-running-process
;; https://www.reddit.com/r/emacs/comments/8kpgot/how_to_start_ansiterm_without_prompt/
;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
(defun dcn/zsh ()
  "Create a Zsh \"ansi-term\"."
  (interactive)
  (ansi-term "/bin/zsh"))

(global-set-key (kbd "C-c z") 'dcn/zsh)

;; TODO See if you can combine these.
;; TODO Get rid of the error you get when killing an ansi-term buffer.
(defun dcn/set-no-process-query-on-exit ()
  "Don't ask before killing the process."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(defun dcn/term-exec-hook ()
  "Kill the buffer when the process is finished."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'dcn/term-exec-hook)
(add-hook 'term-exec-hook 'dcn/set-no-process-query-on-exit)

(use-package diminish
  :ensure t)

(use-package desktop
  :ensure t
  :config
  (desktop-save-mode 1)
  (add-to-list 'desktop-globals-to-save
               'ivy-views))

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1))

(use-package smex
  :ensure t)

;; TODO Set up default workspaces with Ivy view.
(use-package counsel
  :ensure t
  :diminish (ivy-mode counsel-mode)
  :after smex
  :bind (("C-x j" . counsel-mark-ring)
         ("C-s" . counsel-grep-or-swiper)
         ("C-S-s" . swiper-all)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package ivy-xref
  :ensure t
  :after ivy
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package avy
  :ensure t
  :bind  ("C-'" . avy-goto-char-2))

(use-package projectile
  :diminish
  :ensure t
  :init
  (setq projectile-project-search-path '("~/"
                                         "~/go/src/caffeine.tv/"
                                         "~/go/src/github.com/caffeinetv/"
                                         "~/dcn/"
                                         "~/Google Drive/")
        projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :bind ("C-c p" . projectile-command-map)
  :init
  (counsel-projectile-mode 1))

;; TODO ivy integration with magit
;; TODO Magithub
;; TODO Pull requests
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package company
  :diminish
  :after smartparens
  :ensure t
  :functions dcn/kill-region-or-word
  :bind (:map company-active-map
              ("C-w" . dcn/kill-region-or-word)
              ("C-c C-w" . company-show-location))
  :preface
  (defun company-backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-backends (mapcar #'company-backend-with-yas company-backends)))

(use-package company-quickhelp
  :diminish
  :ensure t
  :hook (company-mode . company-quickhelp-mode))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :diminish
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(javascript-eslint javascript-jshint))
  :config
  (global-flycheck-mode))

(use-package flyspell
  :diminish
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1))

(use-package evil
  :ensure t
  :diminish (undo-tree-mode)
  :bind ("C-c e" . evil-mode)
  :init
  (setq evil-want-C-u-scroll t))

(use-package evil-matchit
  :ensure t
  :diminish
  :hook (evil-mode . evil-matchit-mode))

(use-package evil-surround
  :ensure t
  :diminish
  :hook (evil-mode . evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :diminish
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-cleverparens
  :ensure t
  :diminish
  :hook (evil-mode . evil-cleverparens-mode))

(use-package aggressive-indent
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode)))

;; TODO Make smartparens have C-arrow like Paredit.
(use-package smartparens
  :ensure t
  :diminish
  :bind ("C-w" . dcn/kill-region-or-word)
  :preface
  ;; https://emacs.stackexchange.com/questions/28543/smartparens-strict-mode-c-w-kill-line-if-no-active-region
  (defun dcn/kill-region-or-word (&optional arg)
    "Kill active region or one word backward with optional ARG."
    (interactive "p")
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (if smartparens-strict-mode
          (sp-backward-kill-word arg)
        (backward-kill-word arg))))
  :config
  (smartparens-global-strict-mode))

;; TODO Silence the warning at the end.
(use-package undo-tree
  :functions global-undo-tree-mode
  :config
  (global-undo-tree-mode -1))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package css-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package eldoc
  :ensure t
  :diminish)

;; TODO This will recenter the buffer sometimes.
(use-package format-all
  :ensure t
  :diminish
  :hook (python-mode . format-all-mode))

;; TODO Guru
(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports"))

(use-package company-go
  :after company
  :ensure t
  :functions company-backend-with-yas
  :defines command-go-gocode-command
  :init (setq command-go-gocode-command "gocode")
  :config
  (add-to-list 'company-backends
               (company-backend-with-yas 'company-go)))

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(use-package js2-mode
  :ensure t
  :mode "\\.js"
  :config
  (setq js2-strict-missing-semi-warning nil
        js2-basic-offset 2))

(use-package json-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package org
  :hook ((org-mode . org-indent-mode)
	 (org-mode . auto-fill-mode))
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :init
  (setq org-use-speed-commands t
        org-directory "~/Google Drive/org/"
        org-default-notes-file (concat org-directory "todo.org")))

(use-package org-journal
  :ensure t
  :after org
  :init
  (setq org-journal-dir (concat org-directory "journal/")))

;; TODO Would be nice to have async buffer fixing.
(use-package prettier-js
  :ensure t
  :diminish
  :hook (((js2-mode json-mode rjsx-mode web-mode) . prettier-js-mode))
  :config
  (setq prettier-js-command "prettier-standard"))

(use-package restclient
  :ensure t)

(use-package rjsx-mode
  :ensure t)

;; TODO Install Postgres formatter or SQL formatter.
(use-package sqlup-mode
  :ensure t
  :hook (sql-mode . sqlup-mode))

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sql-indent-mode))

(use-package terraform-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.\\(html\\|erb\\)")

(use-package yaml-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
