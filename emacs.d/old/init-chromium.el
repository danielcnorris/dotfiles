;;; init-chromium.el --- Allow emacs to launch Chromium
;;; Commentary:
;;; Code:

(defun dcn/launch-chromium ()
    "Launch Chromium in Emacs."
    (interactive)
    (browse-url "http://google.com"))

;;(setq shr-external-browser "chromium-browser")
;;(global-set-key "\C-ci" 'dcn/launch-chromium)

(provide 'init-chromium)
;;; init-chromium.el ends here
