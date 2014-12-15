;;; init-flyspell.el --- set up spell check
;;; Commentary:
;;; Code:

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell)

(defun dcn/switch-dictionary-ru ()
    "Toggle dictionary between Russian and English."
    (interactive)
    (let* ((dict ispell-current-dictionary)
           (change (if (string= dict "ru") "english" "ru")))
        (ispell-change-dictionary change)
        (message "Dictionary changed from %s to %s" dict change)))

(setq ispell-program-name "aspell")
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--sug-mode=ultra"))

(setq default-input-method "cyrillic-yawerty")
(add-hook 'set-language-environment-hook 'dcn/switch-dictionary-ru)

(defun dcn/toggle-input-change-dictionary ()
    (interactive)
    (toggle-input-method)
    (dcn/switch-dictionary-ru))

(global-set-key (kbd "C-c \\") 'dcn/toggle-input-change-dictionary)

(setq ispell-personal-dictionary dcn/dcn-en-dict)
(provide 'init-flyspell)
;;; init-flyspell.el ends here
