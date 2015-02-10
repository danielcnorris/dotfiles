;;; init-scala.el --- scala settings
;;; Commentary:
;;; Code:

(require-package 'scala-mode2)
;;(setq scala-indent:default-run-on-strategy nil)
;;(setq scala-indent:eager-strategy t)
(setq scala-indent:indent-value-expression t)
(setq scala-indent:align-parameters t)
(setq scla-indent:align-forms t)

(require-package 'sbt-mode)

(provide 'init-scala)
;;; init-scala.el ends here
