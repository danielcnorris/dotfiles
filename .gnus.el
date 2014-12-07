;;; .gnus.el --- Summary
;;; Commentary:
;; Adapted heavily from
;; http://blog.binchen.org/posts/notes-on-using-gnus.html
;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
;; http://stackoverflow.com/questions/20612525/gnus-email-multiple-outgoing-accounts-from-the-same-server-without-imap
;; http://linil.wordpress.com/2008/01/18/gnus-gmail/
;; NOTE: Must set an alias smtp.gmail2.com in /etc/hosts in order to
;; send mail from 2 gmail accounts
;;; Code:

(require 'smtpmail)
(require 'cl)

;; Load account info
(require 'secrets)

;; Add home and work accounts to news
(setq gnus-select-method '(nnimap "home"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream-ssl)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "work"
              (nnimap-address "imap.gmail.com")
              (nnimap-server-port 993)
              (nnimap-stream-ssl)))

;; Favor plaintext
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Change the from address based on which group we enter
(setq gnus-posting-styles
      '(("Gmail" (address dcn/home-address))
        ("^INBOX" (address dcn/home-address))
        ("drafts" (address dcn/home-address))
        ("archive" (address dcn/qlabs-address))
        ("work" (address dcn/qlabs-address))))

;; These function handle choosing the right smtp server to send mail
(defvar smtp-accounts
    `((ssl ,dcn/home-address "smtp.gmail.com" 587 "key" nil)
      (ssl ,dcn/qlabs-address "smtp.gmail2.com" 587 "key" nil)))

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
    "Set related SMTP variables for supplied parameters."
    (setq smtpmail-smtp-server server
          smtpmail-smtp-service port
          smtpmail-auth-credentials (list (list server port user
                                                password))
          smtpmail-auth-supported (list mech)
          smtpmail-starttls-credentials nil)
    (message "Setting SMTP server to '%s:%s' for user '%s'." server
             port user))

(defun set-smtp-ssl (server port user password &optional key cert)
    "Set related SMTP and SSL variables for supplied parameters."
    (setq starttls-use-gnutls t
          starttls-gnutls-program "gnutls-cli"
          starttls-extra-arguments nil
          smtpmail-smtp-server server
          smtpmail-smtp-service port
          smtp-starttls-credentials (list (list server port key cert))
          smtpmail-auth-credentials dcn/auth-info)
    (message
     "Setting SMTP server to '%s:%s' for user '%s'. (SSL enabled)"
     server port user))

(defun change-smtp ()
    "Change the SMTP server accordign to the current from line."
    (save-excursion
        (loop with from = (save-restriction
                              (message-narrow-to-headers)
                              (message-fetch-field "from"))
              for (auth-mech address . auth-spec) in smtp-accounts
              when (string-match address from)
              do (cond
                  ((memq auth-mech '(cram-md5 plain login))
                   (return (apply 'set-smtp (cons auth-mech
                                                    auth-spec))))
                  ((eql auth-mech 'ssl)
                   (return (apply 'set-smtp-ssl auth-spec)))
                  (t (error "Unrecognized SMTP auth. mechanism: '%s'."
                              auth-mech)))
              finally (error "Cannot infer SMTP information."))))

;; Change smtp server on send
(add-hook 'message-send-hook 'change-smtp)

;; Display messages in descending chronological order
(setq-default gnus-summary-line-format
              "%U%R%z %(%&user-date; %-15,15f %B%s%)\n"
              gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
              gnus-summary-thread-gathering-function
              'gnus-gather-threads-by-references)

(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date)
                                   (not gnus-thread-sort-by-number)))

;; Spell check messages
(add-hook 'message-mode-hook 'flyspell-mode)

;; Offline reading
(setq gnus-use-cache t)

;; Fetch only part of the article if possible
(setq gnus-read-active-file 'some)

;; Tree view for groups
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Use threads
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; Only show top level message
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

(provide '.gnus.el)
;;; .gnus.el ends here
