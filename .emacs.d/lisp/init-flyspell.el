;;; init-flyspell.el --- set up spell check
;;; Commentary:
;;; Code:

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell)

(provide 'init-flyspell)
;;; init-flyspell.el ends here
