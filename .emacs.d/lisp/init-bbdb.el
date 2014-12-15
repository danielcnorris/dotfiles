;;; init-bbdb.el --- Settings for BBDB
;;; Commentary:
;;; Code:
(require-package 'bbdb)
(require 'bbdb)

(add-to-list 'load-path "~/.emacs.d/lisp/gmail2bbdb/")
(setq gmail2bbdb-bbdb-file dcn/bbdb-file)
(autoload 'gmail2bbdb-import-file "gmail2bbdb")

(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq bbdb-file dcn/bbdb-file)

(setq bbdb-complete-name-full-completion t)
(setq bbdb-complete-type 'primary-or-name)
(setq bbdb-complete-name-allow-cycling t)


;; http://emacs-fu.blogspot.com.au/2009/08/managing-e-mail-addresses-with-bbdb.html
(setq bbdb-offer-save 1
      bbdb-use-popup t
      bbdb-use-electric-p t
      bbdb-popup-target-lines 1

      bbdb-dwim-net-address-allow-redundancy t
      bbdb-quiet-about-name-mismatches 2

      bbdb-always-add-address t
      bbdb-canonicalize-redundant-nets-p t

      bbdb-completion-type nil

      bbdb-complete-name-allow-cycling t

      bbdb-message-caching-enabled t
      bbdb-use-alternate-names t

      bbdb-elided-display t

      bbdb/mail-auto-create-p
      'bbdb-ignore-some-messages-hook
      bbdb-ignore-some-messages-alist

      '(("From" .
         "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

(setq compose-mail-user-agent-warnings-nil)
(provide 'init-bbdb)
;;; init-bbdb.el ends here
