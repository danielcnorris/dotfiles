;;; package --- summary:
;;; Commentary:
;;; With lots of help from the community.
;;; Code:


;;;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t
      use-package-compute-statistics t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package use-package-ensure-system-package
  :after exec-path-from-shell
  :functions (use-package-ensure-system-package-exists?)
  :config
  ;; If this gets set earlier, Emacs tries to use Tramp.
  (setq use-package-always-defer t))

(use-package diminish)

(use-package auto-package-update
  :defines (auto-package-update-delete-old-versions
            auto-package-update-hide-results)
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))


;;;; Files
(setq custom-file (concat user-emacs-directory "custom.el")
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))


;;;; Appearance
(setq disabled-command-function nil
      inhibit-startup-message t
      initial-scratch-message ""
      ring-bell-function 'ignore)
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

;; NOTE Requires extra work on macos.
;; https://emacs.stackexchange.com/questions/16818/cocoa-emacs-24-5-font-issues-inconsolata-dz/29397#29397
(set-frame-font "InconsolataG 13" nil t)

(use-package hl-todo
  :hook (prog-mode . global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smooth-scrolling
  :hook (after-init . smooth-scrolling-mode))

(use-package zenburn-theme
  :demand
  :config
  (load-theme 'zenburn t)
  ;; Change the mode line emphasis for easier eyebrowse usage.
  (set-face-attribute 'mode-line-emphasis nil :foreground "#DCA3A3"))


;;;; Window Management
(use-package ace-window
  :bind ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :config
  (eyebrowse-setup-opinionated-keys))


;;;; Completion
(global-set-key (kbd "M-m") 'execute-extended-command)
(global-set-key (kbd "M-M") 'back-to-indentation)

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package prescient
  :hook (after-init . prescient-persist-mode))

(use-package counsel
  :diminish (ivy-mode counsel-mode)
  :bind (("C-x j" . counsel-mark-ring)
         ("C-s" . swiper)
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

(use-package ivy-hydra
  :after ivy)

(use-package ivy-prescient
  :after ivy
  :hook (ivy-mode . ivy-prescient-mode))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode))

(use-package helm
  :bind (:map helm-map
              ("C-w" . backward-kill-word)))

(use-package company
  :demand
  :diminish
  :after (smartparens yasnippet)
  :functions (dcn/sp-kill-region-or-word)
  :bind (:map company-active-map
              ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1)))
              ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
              ("C-w" . dcn/sp-kill-region-or-word)
              ("C-c C-w" . company-show-location))
  :preface
  (defun dcn/company-backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t
        company-backends (mapcar #'dcn/company-backend-with-yas company-backends)))

(use-package company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(use-package flx
  :demand)

(use-package company-flx
  :after (company flx)
  :hook (company-mode . company-flx-mode))

;; TODO Eldoc is annoying when using Swiper.
(use-package eldoc
  :diminish)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook (after-init . yas-global-mode)
  :init
  (setq yas-verbosity 2))

(use-package yasnippet-snippets)


;; Project management
(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-project-search-path '("~/"
                                         "~/go/src/caffeine.tv/"
                                         "~/go/src/github.com/caffeinetv/"
                                         "~/dcn/"
                                         "~/Google Drive/")
        projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after projectile
  :hook (after-init . counsel-projectile-mode)
  :bind ("C-c p" . projectile-command-map))

(use-package xref
  :ensure nil
  :config
  ;; Don't use Etags backend.
  (setq xref-backend-functions nil))


;;;; Corrections
(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode))

(use-package flyspell
  :diminish
  :ensure-system-package aspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))


;;;; Editing
(global-set-key (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

(defun dcn/backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg))
  (indent-according-to-mode))

(define-key global-map (kbd "C-u") 'dcn/backward-kill-line)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-auto-revert-mode)
(delete-selection-mode)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      require-final-newline t)

(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode)))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode))

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package evil
  :diminish (undo-tree-mode)
  :bind ("C-c e" . evil-mode)
  :init
  (setq evil-want-C-u-scroll t))

(use-package evil-commentary
  :diminish
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-matchit
  :diminish
  :hook (evil-mode . evil-matchit-mode))

(use-package evil-numbers
  :after evil
  :functions (evil-numbers/inc-at-pt)
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ;; Close enough to C-x without messing up anything important.
              ("C-z" . evil-numbers/dec-at-pt)))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :defines (evil-snipe-local-mode
            evil-snipe-override-local-mode
            evil-snipe-parent-transient-map)
  :init
  (setq evil-snipe-scope 'whole-buffer
        evil-snipe-repeat-scope 'whole-buffer)
  :hook ((evil-mode . evil-snipe-mode)
         (evil-mode . evil-snipe-override-mode)))

;; NOTE cs" doesn't work like in Vim, but looks for next set of quotes instead.
(use-package evil-surround
  :diminish
  :hook (evil-mode . evil-surround-mode))

(use-package lispyville
  :diminish
  :hook (evil-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w slurp/barf-lispy wrap)))

(use-package saveplace
  :ensure nil
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

;; TODO https://github.com/Fuco1/smartparens/issues/80
(use-package smartparens
  :diminish
  :bind ("C-w" . dcn/sp-kill-region-or-word)
  :functions (sp-kill-region sp-backward-kill-word)
  :hook (after-init . smartparens-global-strict-mode)
  :preface
  ;; https://emacs.stackexchange.com/questions/28543/smartparens-strict-mode-c-w-kill-line-if-no-active-region
  (defun dcn/sp-kill-region-or-word (&optional arg)
    "Kill active region or one word backward with optional ARG."
    (interactive "p")
    (require 'smartparens)
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (if smartparens-strict-mode
          (sp-backward-kill-word arg)
        (backward-kill-word arg))))
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode))

;; TODO Better binding for redo.
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `((".*" . ,(concat user-emacs-directory "undo")))))


;;;; Language
(use-package cider)

(use-package clojure-mode)

(use-package css-mode)

(use-package dockerfile-mode)

(use-package gitignore-mode)

;; TODO Use dlv debugger.
;; TODO M-q or gqap doesn't work for comments.
;; TODO Use the test generation package.
;; TODO Text objects for functions like in vim-go.
(use-package go-mode
  :ensure-system-package
  ((gocode . "go get -u github.com/mdempsky/gocode")
   (godef . "go get -u github.com/rogpeppe/godef")
   (godoc . "go get -u golang.org/x/tools/cmd/godoc")
   (goimports . "go get -u golang.org/x/tools/cmd/goimports"))
  :hook (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports"))

(use-package go-tag
  :ensure-system-package
  (gomodifytags . "go get -u github.com/fatih/gomodifytags")
  :bind (:map go-mode-map
              ("C-c t" . go-tag-add)
              ("C-c T" . go-tag-remove)))

;; TODO Tern for JS.
(use-package js2-mode
  :mode "\\.js"
  :config
  (setq js2-strict-missing-semi-warning nil
        js2-basic-offset 2))

;; TODO Make this faster / async.
(use-package prettier-js
  :diminish
  :defines prettier-js-command
  :hook (((js2-mode json-mode rjsx-mode web-mode) . prettier-js-mode))
  :config
  (setq-default flycheck-disabled-checkers '(javascript-eslint javascript-jshint))
  (setq prettier-js-command "prettier-standard"))

(use-package rjsx-mode)

(use-package json-mode)

(use-package markdown-mode)

(use-package vmd-mode
  :ensure-system-package
  (vmd . "npm i -g vmd"))

;; TODO Finish setup and do ensure-system-package
;; pip install jedi
;; pip install flake8
;; pip install black
;; Disable flymake
(use-package elpy
  :config
  (elpy-enable))

;; TODO Rubocop
;; TODO Ruby pair closing.
(use-package robe)

(use-package sqlup-mode
  :diminish
  :hook (sql-mode . sqlup-mode))

(use-package terraform-mode)

;; M-q or gqap doesn't work for comments.
(use-package vimrc-mode)

(use-package web-mode
  :mode "\\.\\(html\\|erb\\)")

(use-package yaml-mode)


;;;; Tools
;; TODO Ensure sqlite3
(use-package helm-dash
  :functions (helm-dash-at-point)
  :bind ("C-c d" . helm-dash-at-point)
  :config
  ;; TODO Set these intelligently based on the mode.
  ;; TODO How to automatically install default docsets.
  (setq  helm-dash-common-docsets '("Clojure" "Emacs_Lisp" "Go" "Javascript" "Python_3" "React" "Ruby" "Ruby_on_Rails_5")
         helm-dash-browser-func 'eww))

(use-package magit
  :after ivy
  :bind (("C-x g" . magit-status)
         ("C-c g s" . magit-status))
  :init
  (setq magit-auto-revert-mode nil))

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package browse-at-remote
  :bind ("C-c g b" . browse-at-remote))

(use-package magithub
  :after magit
  :defines magithub-clone-default-directory
  :init
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/"))

;; Properly encrypt credentials.
;; (use-package mu4e
;;   :ensure-system-package
;;   ;; TODO Make cross-platform.
;;   ((mu . "brew install mu --with-emacs")
;;    (offlineimap . "brew install offlineimap"))
;;   :init
;;   (provide 'html2text))

;; TODO Set up backup / personal files.
(use-package org
  :diminish (org-indent-mode auto-fill-mode)
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :defines (org-capture-templates)
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :init
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-use-speed-commands t
        org-directory "~/Google Drive/org/"
        ;; TODO Pull key files out into constants.
        org-default-notes-file (concat org-directory "todo.org")
        org-agenda-files `(,(concat org-directory "todo.org"))
        org-todo-keywords '((sequence "TODO" "DONE"))
        org-log-done t
        org-tag-alist '(("LATER" . ?l) ("MAYBE" . ?m) ("WAITING" . ?w) ("CANCELED" . ?c))
        org-capture-templates  '(("t" "Todo" entry (file+headline (lambda () (concat org-directory "todo.org")) "To Do")
                                  "* TODO %?\n %a")
                                 ("j" "Journal" entry (file (lambda () (concat org-directory "journal.org")))
                                  "* %?\n %U\n" :prepend t)
                                 ("n" "Note" entry (file (lambda () (concat org-directory "notes.org")))
                                  "* %?\n %U\n" :prepend ))))
;; TODO Backup password store in git
;; TODO pass-otp
;; TODO Easy generate password like in spacemacs.
(use-package pass
  :ensure-system-package
  (pass . "brew install pass"))

(use-package helm-system-packages)

(use-package restart-emacs)

(use-package restclient)

(use-package term
  :ensure f
  :preface
  ;; https://emacs.stackexchange.com/questions/17005/killing-ansi-term-says-has-a-running-process
  ;; https://www.reddit.com/r/emacs/comments/8kpgot/how_to_start_ansiterm_without_prompt/
  ;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
  (defun dcn/zsh ()
    "Create a Zsh \"ansi-term\"."
    (interactive)
    (ansi-term "/bin/zsh"))

  (defun dcn/term-exec-hook ()
    "Kill the buffer when the process is finished."
    (let* ((buff (current-buffer))
           (proc (get-buffer-process buff)))
      ;; TODO Still gives "selecting deleted buffer" error.
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))
      (set-process-sentinel
       proc
       `(lambda (process event)
          (if (string= event "finished\n")
              (kill-buffer ,buff))))))
  :bind ("C-c m" . dcn/zsh)
  :hook (term-exec . dcn/term-exec-hook))


(provide 'init)
;;; init.el ends here
