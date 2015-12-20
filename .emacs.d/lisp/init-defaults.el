;;; init-defaults.el --- main defaults
;;; Commentary:
;;; Code:
(when (eq system-type 'darwin)
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

;; (setq-default auto-fill-function 'do-auto-fill)
;; (set-fill-column 78)

;; ;; Use xclip for copy and paste in X
;; (require-package 'xclip)
;; (xclip-mode 1)

;; ;; Latex preview mode
;; (require-package 'latex-preview-pane)
;; (latex-preview-pane-enable)

;; Clean up auto save and backup files
; (setq backup-directory-alist
;       `((".*" . ,"~/.backups")))
; (setq auto-save-file-name-transforms
;       `((".*" ,"~/.saves" t)))

;; Visual line wrapping
; (global-visual-line-mode 1)

; ;; Tabs are illegal
; (setq-default tab-width 4)
; (add-hook 'before-save-hook
;           (lambda () (untabify (point-min) (point-max))))


; ;; Automatically insert and highlight matching paren, brace, etc.
; (require 'paren)
; (electric-pair-mode 1)
; (show-paren-mode 1)
; (set-face-background 'show-paren-match "#657b83")
; (set-face-foreground 'show-paren-match "#dc322f")
; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

; ;; Delete trailing whitespace on save
; (add-hook 'before-save-hook 'delete-trailing-whitespace)
; (setq require-final-newline 't)

;; Line and column numbers
;; (require 'linum)
;; (global-linum-mode 1)
;; (setq linum-format
;;     (lambda (line) (propertize
;;                     (format (let ((w (length (number-to-string
;;                                               (count-lines (point-min)
;;                                                            (point-max))))))
;;                     (concat " %" (number-to-string w) "d ")) line)
;;                     'face 'linum)))
;; (column-number-mode 1)

;; Don't show linum in text or w3m
;; (add-hook 'text-mode-hook
;;           (lambda () (linum-mode -1)))

;; (add-hook 'w3m-mode
;;           (lambda () (linum-mode -1)))

;; (add-hook 'fundamental-mode
;;           (lambda () (linum-mode -1)))

;; Continuous scroll in doc-vew
; (setq doc-view-continuous t)

;; Reload files updated on disk
; (global-auto-revert-mode 1)

(provide 'init-defaults)
;;; init-defaults ends here
