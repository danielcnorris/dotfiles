;;; init-ledger.el --- Ledger settings
;;; Commentary:
;;; Code:

(require-package 'ledger-mode)
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(provide 'init-ledger)
;;; init-ledger.el ends here
