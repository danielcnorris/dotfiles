;;; package --- summary:
;;; Commentary:
;;; With lots of help from the community.
;;; Code:


;;;; Package
;; HACK In order to get all packages to load.
;; https://github.com/bbatsov/prelude/issues/1225
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

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
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
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
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

(use-package hl-todo
  :hook (prog-mode . global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package zenburn-theme
  :demand
  :config
  (load-theme 'zenburn t))


;;;; Window Management
(use-package windmove
  :demand
  :ensure nil
  :config
  (windmove-default-keybindings))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

;;;; Completion
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

(use-package ivy-prescient
  :after ivy
  :hook (ivy-mode . ivy-prescient-mode))

(use-package eldoc
  :diminish)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook (after-init . yas-global-mode)
  :defines (yas-verbosity)
  :init
  (setq yas-verbosity 2))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet)


;;;; Project Management
(use-package projectile
  :diminish
  :demand
  :config
  (setq projectile-project-search-path (if (eq system-type 'darwin)
                                           '("~/"
                                             "~/go/src/github.com/caffeinetv/"
                                             "~/dcn/"
                                             "~/Google Drive/")
                                         '("~/"))
        projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after projectile
  :hook (after-init . counsel-projectile-mode)
  :bind ("C-c p" . projectile-command-map))


;;;; Corrections
(use-package flycheck
  :disabled
  :diminish
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-highlighting-mode nil
        flycheck-indication-mode nil))

(use-package flyspell
  :diminish
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))


;;;; Editing
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      require-final-newline t)

(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode)
         (racket-mode . aggressive-indent-mode)))

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package dtrt-indent
  :diminish
  :hook (after-init . dtrt-indent-global-mode))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode))

(use-package electric
  :diminish
  :ensure nil
  :hook ((after-init . electric-pair-mode)
         (prog-mode . electric-indent-mode)))

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

(use-package evil-surround
  :diminish
  :hook (evil-mode . evil-surround-mode))

(use-package format-all
  :diminish
  :hook ((python-mode . format-all-mode)
         (js2-mode . format-all-mode)))

(use-package paredit
  :diminish
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (racket-mode . paredit-mode)))

(use-package paren
  :diminish
  :hook (prog-mode . show-paren-mode))

(use-package smartparens
  :hook (elixir-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode))

;;;; Language
(use-package alchemist)

(use-package clojure-mode)

(use-package cider)

(use-package dockerfile-mode)

(use-package elixir-mode
  :hook (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package gitignore-mode)

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports"))

(use-package json-mode)

(use-package js2-mode
  :mode "\\.js"
  :config
  (setq js2-strict-missing-semi-warning nil
        js2-basic-offset 2))

(use-package markdown-mode)

(use-package racket-mode
  :diminish (hs-minor-mode))

(use-package rjsx-mode)

(use-package vimrc-mode)

(use-package web-mode
  :mode "\\.\\(html\\|erb\\)")

(use-package yaml-mode)


;;;; Tools
(use-package magit
  :after ivy
  :bind (("C-x g" . magit-status)
         ("C-c g s" . magit-status)))

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package browse-at-remote
  :bind ("C-c g b" . browse-at-remote))

(use-package pass)

(use-package ivy-pass)

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
