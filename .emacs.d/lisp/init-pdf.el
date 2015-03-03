;;; init-pdf.el --- utilities for pdf viewing
;;; Commentary:
;;; Code:

;; Basic keymaps

;; Disable linum mode
(add-hook 'doc-view-mode-hook
          (lambda ()
              (linum-mode -1)))

(provide 'init-pdf)
;; init-pdf.el ends here
