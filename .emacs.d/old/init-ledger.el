;;; init-ledger.el --- Ledger settings
;;; Commentary:
;;; Code:

(require-package 'ledger-mode)
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(setq ledger-schedule-file (concat dcn/ledger-directory
                                   "ledger-schedule.ledger"))
(add-hook 'ledger-mode-hook
          (lambda ()
              (linum-mode -1)))

(provide 'init-ledger)
;;; init-ledger.el ends here
