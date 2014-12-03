;; Add packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(package-initialize)

;; Load solarized theme
(require-package 'color-theme-solarized)
(load-theme 'solarized-dark t)

;; Remove splash screen
(setq inhibit-startup-message t)

;; Remove menu bar
(menu-bar-mode -1)

;; Tabs are illegal
(setq-default indent-tabs-mode nil)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete trailing newline
(defun my-other-delete-trailing-blank-lines ()
    "Deletes all blank lines at the end of the file, even the last one"
    (interactive)
    (save-excursion
        (save-restriction
            (widen)
            (goto-char (point-max))
            (delete-blank-lines)
            (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
                (if (> trailnewlines 0)
                        (progn
                            (delete-char trailnewlines)))))))

(add-hook 'before-save-hook 'my-other-delete-trailing-blank-lines)

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

;; Org mode setup
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(display-color-cells (selected-frame))
(find-file "~/Dropbox/org/todo.org")

;; Lisp settings
(setq lisp-body-indent 4)

;; Coffeescript settings
(require-package 'coffee-mode)
(custom-set-variables '(coffee-tab-width 4))
