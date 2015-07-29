;;; init-ess.el --- configure ESS
;;; Commentary:
;; Setup for data analysis
;;; Code:

(require-package 'ess)
(require 'ess-site)
(setq ess-eval-visibly nil)
(setq ess-ask-for-ess-directory nil)
(setq auto-mode-alist
      (append '(("\\.r" . R-mode))
              auto-mode-alist))

(add-hook 'ess-mode-hook
          (lambda () (flycheck-mode -1)))
(add-hook 'inferior-ess-mode-hook
          (lambda () (linum-mode -1)))

(ess-toggle-underscore nil)
(provide 'init-ess)
;; init-ess.el ends here
