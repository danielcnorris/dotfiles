;;; init-javascript.el --- javascript settings
;;; Commentary:
;;; Code:

(require-package 'coffee-mode)
(custom-set-variables '(coffee-tab-width 4))

(require-package 'js2-mode)
(require-package 'web-mode)
(require-package 'json-mode)
(require-package 'exec-path-from-shell)


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(setq javascript-indent-level 2)
(setq js-indent-level 2)
(setq coffee-tab-width 2)
(setq js2-basic-offset 2)js-indent-level
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

(provide 'init-javascript)
;;; init-javascript.el ends here
