;;; init-html.el --- config html
;;; Commentary:
;;; Code:

(add-hook 'html-mode-hook
          (lambda ()
              (set (make-local-variable 'sgml-basic-offset) 4)))

(provide 'init-html)
;;; init-html.el ends here
