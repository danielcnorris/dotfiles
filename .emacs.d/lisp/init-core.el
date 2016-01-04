;;; init-core.el --- main settings
;;; Commentary:
;;; Code:
(when (eq system-type 'darwin)
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (setq mac-command-modifier 'meta))

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-frame-parameter nil 'fullscreen 'fullboth)

(display-time-mode 1)
(display-battery-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(line-number-mode 1)
(column-number-mode 1)

(global-auto-revert-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory
                        "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory
                        "saves") t)))

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

(require-package 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char)

(require-package 'which-key)
(which-key-mode)

(require-package 'smartparens)
(smartparens-global-mode 1)
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil))
(show-smartparens-global-mode 1)

(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "C-'") 'comment-or-uncomment-region)

(setq-default fill-column 78)
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)
(global-visual-line-mode 1)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook
          (lambda () (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(setq doc-view-continuous t)

(require 'flyspell)
(require-package 'helm-flyspell)
(define-key flyspell-mode-map (kbd "C-,") 'helm-flyspell-correct)
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; http://oremacs.com/2015/01/01/three-ansi-term-tips/
(defvar dcn/term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list dcn/term-shell)))
(ad-activate 'ansi-term)
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-y") 'term-paste))
(defun dcn/term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'dcn/term-exec-hook)
(setq explicit-shell-file-name "/bin/bash")
(global-set-key (kbd "C-c t") 'ansi-term)

(provide 'init-core)
;;; init-defaults ends here
