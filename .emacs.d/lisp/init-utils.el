;;; init-utils.el --- General config utils
;;; Commentary:
;;; Code:

(defalias 'after-load 'with-eval-after-load)
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(provide 'init-utils)
;;; init-utils.el ends here
