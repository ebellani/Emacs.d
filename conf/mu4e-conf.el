(require 'jl-encrypt)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
;; https://gist.github.com/areina/3879626

(require 'mu4e)

;; org mode linking support
(require 'org-mu4e)

(defvar *authinfo-file-path* (expand-file-name "~/.authinfo.gpg"))

;; default
(setq mu4e-maildir (expand-file-name "~/Mail/mbsyncmail/"))

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-attachment-dir  "~/Downloads")

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"          . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/hackers-neoway" . ?n)))


(require 'mu4e-contrib)
;; general config
(setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc gmail"
      mu4e-html2text-command 'mu4e-shr2text
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "ebellani@gmail.com"
 user-full-name    "Eduardo Bellani"
 mu4e-compose-signature "Eduardo Bellani")

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("mail.example.com" 587 nil nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; Tells Gnus to inline the part
(eval-after-load "mm-decode"
                 '(add-to-list 'mm-inlined-types "application/pgp$"))
;; Tells Gnus how to display the part when it is requested
(eval-after-load "mm-decode"
                 '(add-to-list 'mm-inline-media-tests '("application/pgp$"
                                                        mm-inline-text identity)))
;; Tell Gnus not to wait for a request, just display the thing
;; straight away.
(eval-after-load "mm-decode"
                 '(add-to-list 'mm-automatic-display "application/pgp$"))
;; But don't display the signatures, please.
(eval-after-load "mm-decode"
                 (quote (setq mm-automatic-display (remove "application/pgp-signature"
                                                           mm-automatic-display))))

(setq epa-file-cache-passphrase-for-symmetric-encryption t)


;; set helm support
(require 'helm-mu)
