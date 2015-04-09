(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
;; https://gist.github.com/areina/3879626

(require 'mu4e)

(defvar *authinfo-file-path* (expand-file-name "~/.authinfo.gpg"))

;; default
(setq mu4e-maildir (expand-file-name "~/Mail/ebellani"))

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-attachment-dir  "~/Downloads")

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/sent"  . ?s)
        ("/trash" . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap"
      ;; mu4e-update-interval  120
      )

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "ebellani@gmail.com"
 user-full-name    "Eduardo Bellani"
 mu4e-compose-signature
 (concat
  "--\n"
  "Eduardo Bellani"))

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      *authinfo-file-path*
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

(defun offlineimap-get-password (host port login)
  "http://www.emacswiki.org/emacs/OfflineIMAP This sends the
password back to offlineimap from the encrypted .authinfo file"
  (funcall (plist-get (car (auth-source-search
                            :host host
                            :port port
                            :user login
                            :require (and '(:user :secret)))) :secret)))
