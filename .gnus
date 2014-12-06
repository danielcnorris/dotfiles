;; Gnus config
;; Adapted heavily from
;; http://blog.binchen.org/posts/notes-on-using-gnus.html
;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
;; http://stackoverflow.com/questions/20612525/gnus-email-multiple-outgoing-accounts-from-the-same-server-without-imap
;; http://linil.wordpress.com/2008/01/18/gnus-gmail/
;; NOTE: Must set an alias smtp.gmail2.com in /etc/hosts in order to
;; send mail from 2 gmail accounts

(require 'smtpmail)
(require 'cl)

(setq gnus-select-method '(nnimap "home"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream-ssl)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "work"
              (nnimap-address "imap.gmail.com")
              (nnimap-server-port 993)
              (nnimap-stream-ssl)))

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-posting-styles
      '(("Gmail" (address "danielcnorris@gmail.com"))
        ("^INBOX" (address "danielcnorris@gmail.com"))
        ("drafts" (address "danielcnorris@gmail.com"))
        ("archive" (address "daniel@quantifiedlabs.com"))
        ("work" (address "daniel@quantifiedlabs.com"))))


(defvar smtp-accounts
    '((ssl "danielcnorris@gmail.com" "smtp.gmail.com" 587 "key" nil)
      (ssl "daniel@quantifiedlabs.com" "smtp.gmail2.com" 587 "key" nil)))

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
          smtpmail-auth-credentials "~/.authinfo")
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
              finally (error "Bad '%s' '%s '%s '%s'" address from
                             auth-mech auth-spec))))

(add-hook 'message-send-hook 'change-smtp)

(setq-default gnus-summary-line-format
              "%U%R%z %(%&user-date; %-15,15f %B%s%)\n"
              gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
              gnus-summary-thread-gathering-function
              'gnus-gather-threads-by-references)

(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date)
                                   (not gnus-thread-sort-by-number)))

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
