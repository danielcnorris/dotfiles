;;; package --- summary:
;;; Commentary:
;;; With help from Centaur Emacs, Spacemacs, Doom Emacs, and others.
;;; Code:

;; TODO Figure out the tags issue
;; TODO Figure out smartparens issue

;; TODO REPL integration for other languages
;; TODO Offline documentation sets / browsing (e.g. dash)
;; TODO Learn how to use imenu? moccur
;; TODO Encrypt authinfo / mu4e
;; TODO Email
;; TODO Set up personal org files on Dropbox / Android

;; Requirements:
;; Autocomplete
;; xref def and referendes
;; Projectile
;; Flycheck
;; Easy documentation lookup
;; Completion / navigation
;; Evil
;; Yas
;; Multiterm

;; Proposed layout:
;; Foundation
;; Appearance
;; Completion and navigation
;; Editing
;; Language
;; Tools
(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar use-package-always-ensure)
(setq use-package-always-ensure t)

(require 'use-package)
(require 'diminish)

(use-package use-package-ensure-system-package
  :functions (use-package-ensure-system-package-exists?))

(use-package auto-package-update
  :defines (auto-package-update-delete-old-versions
            auto-package-update-hide-results)
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; NOTE Requires extra work on macos.
;; https://emacs.stackexchange.com/questions/16818/cocoa-emacs-24-5-font-issues-inconsolata-dz/29397#29397
(set-frame-font "InconsolataG 13" nil t)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  ;; Change the mode line emphasis for easier eyebrowse usage.
  (set-face-attribute 'mode-line-emphasis nil :foreground "#DCA3A3"))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package better-defaults
  :diminish auto-revert-mode
  :config
  ;; TODO https://github.com/technomancy/better-defaults/pull/25
  (ido-mode -1))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups/") t))
      create-lockfiles nil)

(global-set-key (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

(defun dcn/backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg))
  (indent-according-to-mode))
(define-key global-map (kbd "C-u") 'dcn/backward-kill-line)

(setq inhibit-startup-message t
      initial-scratch-message ""
      ring-bell-function 'ignore
      disabled-command-function nil)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Enable electric pair mode for better indentation within pairs.
(electric-pair-mode)

;; https://emacs.stackexchange.com/questions/17005/killing-ansi-term-says-has-a-running-process
;; https://www.reddit.com/r/emacs/comments/8kpgot/how_to_start_ansiterm_without_prompt/
;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
(defun dcn/zsh ()
  "Create a Zsh \"ansi-term\"."
  (interactive)
  (ansi-term "/bin/zsh"))

(global-set-key (kbd "C-c z") 'dcn/zsh)

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

(add-hook 'term-exec-hook 'dcn/term-exec-hook)

(use-package recentf
  :config
  (recentf-mode))

;; (use-package smex)

;; (use-package counsel
;;   :diminish (ivy-mode counsel-mode)
;;   :after smex
;;   :bind (("C-x j" . counsel-mark-ring)
;;          ("C-s" . counsel-grep-or-swiper)
;;          ("C-S-s" . swiper-all)
;;          ("C-r" . swiper)
;;          ("C-c C-r" . ivy-resume)
;;          ("C-c v" . ivy-push-view)
;;          ("C-c V" . ivy-pop-view)
;;          :map swiper-map
;;          ("M-%" . swiper-query-replace))
;;   :hook ((after-init . ivy-mode)
;;          (ivy-mode . counsel-mode))
;;   :init
;;   (setq ivy-use-virtual-buffers t
;;         ivy-count-format "(%d/%d) "
;;         counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

;; (use-package ivy-xref
;;   :after ivy
;;   :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package ivy-rich
;;   :after ivy
;;   :hook (ivy-mode . ivy-rich-mode))

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; (use-package flyspell-correct-ivy
;;   :bind (:map flyspell-mode-map
;;               ("C-;" . flyspell-correct-wrapper)))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; TODO Don't include TAGS files.
;; (use-package desktop
;;   :config
;;   (desktop-save-mode)
;;   (add-to-list 'desktop-globals-to-save
;;                'ivy-views))

(use-package eyebrowse
  :config
  (eyebrowse-mode)
  (eyebrowse-setup-opinionated-keys))

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package projectile
  :diminish
  :init
  (setq projectile-project-search-path '("~/"
                                         "~/go/src/caffeine.tv/"
                                         "~/go/src/github.com/caffeinetv/"
                                         "~/dcn/"
                                         "~/Google Drive/")
        projectile-tags-backend nil
        projectile-completion-system 'ivy))

;; (use-package counsel-projectile
;;   :after projectile
;;   :bind ("C-c p" . projectile-command-map)
;;   :init
;;   (counsel-projectile-mode))

(use-package magit
  :after ivy
  :bind (("C-x g" . magit-status)
         ("C-c g s" . magit-status))
  :init
  (setq magit-auto-revert-mode nil))

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package browse-at-remote
  :bind ("C-c g g" . browse-at-remote))

(use-package magithub
  :after magit
  :defines magithub-clone-default-directory
  :init
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/"))

(use-package company
  :diminish
  :after (smartparens yasnippet)
  :functions (dcn/sp-kill-region-or-word)
  :bind (:map company-active-map
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

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode))

(use-package flycheck
  :diminish
  :init
  (setq-default flycheck-disabled-checkers '(javascript-eslint javascript-jshint))
  :config
  (global-flycheck-mode))

(use-package flyspell
  :diminish
  :ensure-system-package aspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `((".*" . ,(concat user-emacs-directory "undo")))))

(use-package evil
  :diminish (undo-tree-mode)
  :bind ("C-c e" . evil-mode)
  :init
  (setq evil-want-C-u-scroll t))

(use-package evil-matchit
  :diminish
  :hook (evil-mode . evil-matchit-mode))

;; NOTE cs" doesn't work like in Vim, but looks for next set of quotes instead.
(use-package evil-surround
  :diminish
  :hook (evil-mode . evil-surround-mode))

(use-package evil-commentary
  :diminish
  :hook (evil-mode . evil-commentary-mode))

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

(use-package lispyville
  :diminish
  :hook (evil-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w slurp/barf-lispy wrap)))

(use-package evil-numbers
  :after evil
  :functions (evil-numbers/inc-at-pt)
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ;; Close enough to C-x without messing up anything important.
              ("C-z" . evil-numbers/dec-at-pt)))

(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode)))

;; (use-package smartparens
;;   :diminish
;;   :bind ("C-w" . dcn/sp-kill-region-or-word)
;;   :functions (sp-kill-region sp-backward-kill-word)
;;   :hook (after-init . smartparens-global-strict-mode)
;;   :config
;;   ;; https://emacs.stackexchange.com/questions/28543/smartparens-strict-mode-c-w-kill-line-if-no-active-region
;;   (defun dcn/sp-kill-region-or-word (&optional arg)
;;     "Kill active region or one word backward with optional ARG."
;;     (interactive "p")
;;     (require 'smartparens)
;;     (if (use-region-p)
;;         (sp-kill-region (region-beginning) (region-end))
;;       (if smartparens-strict-mode
;;           (sp-backward-kill-word arg)
;;         (backward-kill-word arg))))
;;   (require 'smartparens-config)
;;   (sp-use-paredit-bindings))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :config
  (yas-global-mode))

(use-package yasnippet-snippets)

(use-package cider)

(use-package clojure-mode)

(use-package css-mode)

(use-package dockerfile-mode)

(use-package eldoc
  :diminish)

(use-package gitignore-mode)

;; TODO Add better snippets.
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

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(use-package js2-mode
  :mode "\\.js"
  :config
  (setq js2-strict-missing-semi-warning nil
        js2-basic-offset 2))

(use-package json-mode)

(use-package markdown-mode)

(use-package vmd-mode
  :ensure-system-package
  (vmd . "npm i -g vmd"))

(use-package org
  :diminish (org-indent-mode auto-fill-mode)
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :defines org-capture-templates
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :init
  (setq org-use-speed-commands t
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

;; TODO Make this faster.
(use-package prettier-js
  :diminish
  :defines prettier-js-command
  :hook (((js2-mode json-mode rjsx-mode web-mode) . prettier-js-mode))
  :config
  (setq prettier-js-command "prettier-standard"))


;; TODO Add async formatting for JavaScript and Ruby.
;; TODO Add Clojure LSP.
;; TODO LSP temporarily breaks Emacs lisp evaluation.
(use-package lsp-mode
  :diminish
  :defines lsp-ui-mode-map)

;; TODO I'd much rather have xref style interface to references.
(use-package lsp-ui
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-mode . flycheck-mode))
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        ;; TODO Still italicizes sometimes.
        lsp-ui-peek-fontify nil))

(use-package company-lsp
  :after company
  :functions dcn/company-backend-with-yas
  :init
  (add-to-list 'company-backends
               (dcn/company-backend-with-yas 'company-lsp)))

(use-package lsp-go
  :ensure-system-package
  (go-langserver . "go get -u github.com/sourcegraph/go-langserver")
  :after lsp-mode
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable))

;; TODO Should I use pyflakes? Make warnings compatible with black.
(use-package lsp-python
  :ensure-system-package
  ((pyls . "pip install 'python-language-server[all]'")
   (black . "pip install black"))
  ;; TODO Make this work.
  ;; (pyls-black . "pip install pyls-black"))
  :after lsp-mode
  :commands lsp-python-enable
  :hook ((python-mode . lsp-python-enable)
         (before-save . lsp-format-buffer)))

(use-package lsp-ruby
  :ensure-system-package
  (solargraph . "sudo gem install solargraph")
  :after lsp-mode
  :commands lsp-ruby-enable
  :hook (ruby-mode . lsp-ruby-enable))

(use-package lsp-javascript-typescript
  :ensure-system-package
  (javascript-typescript-langserver . "npm i -g javascript-typescript-langserver")
  :commands lsp-javascript-typescript-enable
  :hook (js2-mode . lsp-javascript-typescript-enable))

(use-package restclient)

(use-package rjsx-mode)

;; TODO Install Postgres formatter or SQL formatter.
(use-package sqlup-mode
  :diminish
  :hook (sql-mode . sqlup-mode))

(use-package terraform-mode)

(use-package vimrc-mode)

(use-package web-mode
  :mode "\\.\\(html\\|erb\\)")

(use-package yaml-mode)

;; (use-package mu4e
;;   :ensure-system-package
;;   ;; TODO Make cross-platform.
;;   ((mu . "brew install mu --with-emacs")
;;    (offlineimap . "brew install offlineimap"))
;;   :init
;;   (provide 'html2text))

;; TODO Backup password store in git
;; TODO pass-otp
;; TODO Easy generate password like in spacemacs.
(use-package pass
  :ensure-system-package
  (pass . "brew install pass"))

(provide 'init)
;;; init.el ends here
