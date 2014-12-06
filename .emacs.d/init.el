;; Load secrets
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'secrets)

;; Add packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defun dcn/require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (dcn/require-package package min-version t)))))

(package-initialize)

;; Load solarized theme
(dcn/require-package 'color-theme-solarized)
(load-theme 'solarized-dark t)

;; Remove splash screen
(setq inhibit-startup-message t)

;; Remove menu bar
(menu-bar-mode -1)

;; Break lines after 78 chars
(setq-default auto-fill-function 'do-auto-fill)
(set-fill-column 78)

;; Visual line wrapping
(global-visual-line-mode 1)

;; Tabs are illegal
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Automatically insert and highlight matching paren, brace, etc.
(electric-pair-mode 1)
(show-paren-mode 1)
(require 'paren)
(set-face-background 'show-paren-match "#657b83")
(set-face-foreground 'show-paren-match "#dc322f")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Line and column numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format
    (lambda (line) (propertize
                    (format (let ((w (length (number-to-string
                                              (count-lines (point-min)
                                                           (point-max))))))
                    (concat " %" (number-to-string w) "d ")) line)
                    'face 'linum)))
(column-number-mode 1)

;; Autocomplete
(dcn/require-package 'auto-complete)
(require 'auto-complete)
(global-auto-complete-mode t)
(ac-linum-workaround)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'coffee-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)

;; Spellcheck
;; Be sure to enable before opening any buffers
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Org mode setup
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline (concat dcn/org-directory "todo.org") "Tasks")
         "* TODO %?%i\nCREATED: %U")
        ("l" "Todo with link" entry
         (file+headline (concat dcn/org-directory "todo.org") "Tasks")
         "* TODO %?%i\n%a\nCREATED: %U")
        ("j" "Journal" entry
         (file+datetree (concat dcn/org-directory "journal.org"))
         "* %?\n%i\nCREATED: %U")))
(find-file (concat dcn/org-directory "todo.org"))

;; Require flycheck
;;(dcn/require-package 'flycheck)
;;(add-hook 'after-init-hood #'global-flycheck-mode)

;; Lisp settings
(setq lisp-body-indent 4)

;; Coffeescript settings
(dcn/require-package 'coffee-mode)
(custom-set-variables '(coffee-tab-width 4))
