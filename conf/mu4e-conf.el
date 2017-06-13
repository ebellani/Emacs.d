(require 'jl-encrypt)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
;; https://gist.github.com/areina/3879626

(require 'mu4e)

;; org mode linking support
(require 'org-mu4e)

(setq org-mu4e-link-query-in-headers-mode nil)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"   . ?i)
        ("/sent"    . ?s)
        ("/starred" . ?t)
        ("/all"     . ?a)
        ("/hr"      . ?h)))

(require 'mu4e-contrib)

;; general config
(setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc gmail"
      ;;  "html2text -utf8 -width 72" ?
      ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
      ;; stop UID errors
      mu4e-change-filenames-when-moving t
      mu4e-html2text-command 'mu4e-shr2text
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-attachment-dir  "~/Downloads"
      mu4e-maildir (expand-file-name "~/Mail/mail/")
      ;; don't save message to Sent Messages, GMail/IMAP will take care of this
      mu4e-sent-messages-behavior 'delete
      ;; kill buffers on exit
      message-kill-buffer-on-exit t
      ;; show fancy chars
      mu4e-use-fancy-chars t
      ;; attempt to show images when viewing messages sometimes this
      ;; slows down in the case of big djvu files (they are
      ;; interpreted as images).
      mu4e-view-show-images t
      org-mu4e-convert-to-html t)

;; something about ourselves
(setq
 user-mail-address "emb@brickabode.com"
 user-full-name    "Eduardo Bellani"
 mu4e-compose-signature "--\nEduardo Bellani")

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
