;; -*- lexical-binding: t; -*-
;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; https://github.com/cqql/dotfiles/blob/master/home/.emacs.d/init.org

(require 'cl-lib)

(setq lexical-binding t)

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;; path setup

(defcustom filter-bad-contacts
  #'identity
  "This is used to filter the bad contacts that mu4e is
accumulating.")

(defcustom my/path-aliases
  (list :emacs  "~/.emacs.d"
        :srs    "~/.emacs.d"
        :work   "~/.emacs.d"
        :agenda "~/.emacs.d")
  "Location of my paths for ease of usage. Customize for each
  environment if needed.")

(defun my/path (dir &optional subpath)
  "Build a path name. See https://github.com/arecker/emacs.d"
  (let ((dir (file-name-as-directory
              (cl-getf my/path-aliases dir
                       (format "~/%s" dir))))
        (subpath (or subpath "")))
    (concat dir subpath)))

(defcustom main-agenda (my/path :emacs "agenda.org")
  "This is used to store quickly todo items without refiling")

(add-to-list 'load-path (my/path :emacs "lib"))

;; add the custom file inside the emacs folder

(let ((custom-file-path (my/path :emacs "custom.el")))
  (if (file-readable-p custom-file-path)
      (progn
        (setq custom-file custom-file-path)
        (load custom-file))
    (warn "Custom file not found at expected path %s" custom-file-path)))

;;; font family & size

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 80)

;;; things that I don't know how to do with use-package

(setq system-time-locale "pt_BR.UTF-8")

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t
      pop-up-frames nil
      standard-indent 2
      auto-save-no-message t)

(setq-default indent-tabs-mode nil
              fill-column 80)

;; Maps swaps [ for ( and vice versa. I use parens much more than square
;; brackets.
(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

(define-key key-translation-map (kbd "<menu>") (kbd "ESC"))

;;; packages


;;; straight installation
;;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; use-package config way

;; install use-package
;; see http://cachestocaches.com/2015/8/getting-started-use-package/
;; and http://cestlaz.github.io/posts/using-emacs-1-setup/
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("nongnu"     . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"       . "https://orgmode.org/elpa/") t)

(unless package--initialized (package-initialize t))

(eval-when-compile
  (require 'use-package))

;;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;; org auxiliary functions

(defun myorg/numeric-entry-or-zero (pom entry-name)
  (let ((entry (org-entry-get pom entry-name)))
    (if entry (string-to-number entry) 0)))

(require 'calc-ext)

(defun myorg/cmp-wsjf-property (entry-a entry-b)
  "Compare two `org-mode' agenda entries by their WSJF.
If a is before b, return -1. If a is after b, return 1. If they
are equal return t."
  (let* ((getter (lambda (entry)
                   (round (myorg/numeric-entry-or-zero
                           (get-text-property 0 'org-marker entry)
                           "wsjf"))))
         (wsjf-a (funcall getter entry-a))
         (wsjf-b (funcall getter entry-b))
         (cmp (math-compare wsjf-a wsjf-b)))
    (if (zerop cmp)
        nil
      cmp)))

(cl-defun myorg/add-wsjf-to-scope (&optional (scope 'agenda))
  "Tries to add a `wsjf' property to all items in SCOPE. Agenda is the default"
  (interactive)
  (org-map-entries
   (lambda ()
     (condition-case err
         (org-set-property
          "wsjf"
          (format "%.2f"
                  (/ (+ (myorg/numeric-entry-or-zero nil "bv")
                        (myorg/numeric-entry-or-zero nil "tc")
                        (myorg/numeric-entry-or-zero nil "rr-oe"))
                     (myorg/numeric-entry-or-zero nil "eff"))))
       (error (message "%s" (error-message-string err))
              t)))
   nil
   scope))

(use-package shr
  :config
  (setq shr-max-width (- fill-column 10)))

(use-package org
  :bind (("C-c l" . 'org-store-link)
         ("C-c c" . 'org-capture)
         ("C-c a" . 'org-agenda)
         ("C-c b" . 'org-iswitchb))
  :straight t
  :preface
  (setq org-export-backends '(moderncv md gfm beamer ascii taskjuggler html latex odt org))
  :config
  (require 'oc-biblatex)
  (setq org-refile-file-path (my/path :emacs "refile.org")
        org-refile-allow-creating-parent-nodes 'confirm
        org-agenda-cmp-user-defined 'myorg/cmp-wsjf-property
        org-agenda-sorting-strategy
        '((agenda habit-down user-defined-down time-up priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))
        org-babel-inline-result-wrap "%s"
        org-habit-graph-column 60
        org-habit-following-days 0
        org-habit-preceding-days 14
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-tag-alist '((:startgroup)
                        ("noexport" . ?n)
                        ("export" . ?e)
                        (:endgroup))
        org-refile-targets
        `((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 2)
          (,(my/path :srs "deck.org") :maxlevel . 2)
          (,(my/path :work "meetings.org") :maxlevel . 2))
        org-capture-templates
        `(("e" "Email [mu4e]" entry (file main-agenda)
           ,(concat "* TODO Process \"%a\"\n"
                    "SCHEDULED: %t\n"
                    ":LOGBOOK:\n"
                    "- State \"TODO\"       from \"\"  %U  \\\\\n"
                    "  %^{Initial log} %?\n"
                    "  from %:from\n"
                    ":END:"))
          ("t" "todo" entry
           (file main-agenda)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":BV:\n"
                    ":TC:\n"
                    ":RR-OE:\n"
                    ":EFF:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    " - State \"TODO\"       from \"\"  %U  \\\\\n"
                    "  %^{Initial log} %?\n"
                    ":END:"))
          ("w" "work reminder" entry
           (file main-agenda)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: <%%(memq (calendar-day-of-week date) '(1 2 3 4 5))>%?\n"
                    ":PROPERTIES:\n"
                    ":work_reminder: t\n"
                    ":BV:\n"
                    ":TC:\n"
                    ":RR-OE:\n"
                    ":EFF:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    "- Initial note taken on %U \\\n"
                    "%^{Initial note}\n"
                    ":END:\n"))
          ("h" "habit" entry
           (file main-agenda)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %(org-insert-time-stamp nil nil nil nil nil \" +1w\")%?\n"
                    ":PROPERTIES:\n"
                    ":style: habit\n"
                    ":BV:\n"
                    ":TC:\n"
                    ":RR-OE:\n"
                    ":EFF:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    "- State \"TODO\"       from \"\"  %U  \\\\\n"
                    "%^{Initial log}\n"
                    ":END:\n"))
          ("m" "meeting log" entry
           (file ,(my/path :work "meetings.org"))
           ,(concat "* %^{Title}\n"
                    "** Context\n"
                    "%^{Context}\n"
                    "** Goal\n"
                    "%^{Goal}\n"
                    "** Agenda\n"
                    "%^{Agenda}\n"
                    "** Ata\n"
                    "%^{Minutes})\n"))
          ("d" "Drill card with answer" entry
           (file ,(my/path :srs "deck.org"))
           ,(concat "* Item           :drill:\n"
                    "%^{Question}\n"
                    "** Answer\n"
                    "%^{Answer}\n"))
          ("z" "Drill" entry
           (file ,(my/path :srs "deck.org"))
           ,(concat "* Item           :drill:\n"
                    "%?\n"))
          ("x" "Drill cloze 2" entry
           (file ,(my/path :srs "deck.org"))
           ,(concat "* Item           :drill:\n"
                    ":PROPERTIES:\n"
                    ":drill_card_type: hide2cloze\n"
                    ":END:\n"
                    "%?\n")))
        org-todo-keywords
        '((sequence "TODO(t@/!)" "|" "DONE(d@/!)")
          (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
          (sequence "REPEAT(r@/!)"))
        org-imenu-depth 6
        org-src-fontify-natively t
        ;; disable confirmation of evaluation of code. CAREFUL WHEN EVALUATING
        ;; FOREIGN ORG FILES!
        org-confirm-babel-evaluate nil
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{}
        org-babel-default-header-args
        (cons '(:noweb . "yes")
              (assq-delete-all :noweb org-babel-default-header-args))
        org-babel-default-header-args
        (cons '(:tangle . "yes")
              (assq-delete-all :tangle org-babel-default-header-args))
        org-babel-default-header-args
        (cons '(:comments . "link")
              (assq-delete-all :comments org-babel-default-header-args))
        org-duration-format '((special . h:mm))
        org-goto-interface 'outline-path-completion
        ;; agenda stuff copied from
        ;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-block-separator nil
        org-agenda-include-diary nil
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t
        ;; allows multiple agenda views to coexist
        org-agenda-sticky nil ;; setting it to t breaks capture from agenda, for now
        org-agenda-span 'day
        org-plantuml-jar-path "/home/user/bin/plantuml.jar"
        org-latex-pdf-process (list "latexmk -silent -f -pdf %f")
        org-log-reschedule 'note
        org-cite-export-processors '((latex biblatex)
                                     (moderncv basic)
                                     (t basic)))
  (defun my/org-capture-mail ()
    "https://github.com/rougier/emacs-gtd"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "e"))
  ;; format timestamps. See
  ;; http://endlessparentheses.com/better-time-stamps-in-org-export.html
  ;; get images to reload after execution. Useful for things such as
  ;; gnuplot. See https://emacs.stackexchange.com/q/3302
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot     . t)
     (latex   . t)
     (shell   . t)
     (python  . t)
     (js      . t)
     (ditaa   . t)
     (ocaml   . t)
     (java    . t)
     (scheme  . t)
     (plantuml . t)
     (ditaa   . t)
     (sqlite  . t)
     (gnuplot . t)
     (ditaa  . t)
     (C      . t)
     (org    . t)))
  (defun org-set-as-habit ()
    (interactive)
    (org-set-property "STYLE" "habit")))

(use-package re-builder
  :config (setq reb-re-syntax 'string))

(use-package browse-url
  :bind  (:map
          global-map
          ("C-c w" . 'set-browser!))
  :config
  (defun set-browser! (&optional arg)
    "Makes the default browser external or internal by setting the
`browse-url-browser-function' accordingly"
    (interactive "P")
    (setq browse-url-browser-function
          (if (equal arg '(4))
              'eww-browse-url
            'browse-url-chrome))))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(use-package minibuffer
  :config
  (setq completion-styles '(flex))
  (scroll-bar-mode 0))

(use-package mu4e
  :load-path "/opt/mu/mu4e/"
  :bind (:map
         mu4e-headers-mode-map ("C-c i" . 'my/org-capture-mail)
         :map
         mu4e-view-mode-map ("C-c i" . 'my/org-capture-mail))
  :config
  (require 'mu4e-contrib)
  ;; general config
  ;; add encryption to all messages
  (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-encrypt)
  (setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc gmail"
        mu4e-headers-show-threads t
        mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
        mu4e-contact-process-function 'filter-bad-contacts
        ;;  "html2text -utf8 -width 72" ?
        ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
        ;; stop UID errors
        mu4e-change-filenames-when-moving t
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-update-interval 120
        mu4e-headers-auto-update t
        mu4e-attachment-dir  "~/Downloads"
        mu4e-maildir (expand-file-name "~/Mail/")
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
        org-mu4e-convert-to-html t
        mu4e-mu-binary "/opt/mu/mu/mu"
        mu4e-headers-fields '((:human-date   . 12)
                              (:flags        . 6)
                              (:from-or-to   . 22)
                              (:subject))
        mu4e-maildir-shortcuts
        '(("/INBOX" . 105)
          ("/sent" . 115)
          ("/starred" . 116)
          ("/all" . 97))
        ;; There is a new message-view for mu4e, based on the Gnus'
        ;; article-view. This bring a lot of (but not all) of the very rich Gnus
        ;; article-mode feature-set to mu4e, such as S/MIME-support,
        ;; syntax-highlighting,
        mu4e-view-use-gnus nil
        mu4e-date-format-long "%F"
        mu4e-headers-date-format "%F"
        mu4e-headers-time-format "%T"
        mu4e-split-view 'single-window
        mail-user-agent 'mu4e-user-agent
        mu4e-hide-index-messages t
        mu4e-org-contacts-file (my/path :agenda "contacts.org"))
  (unintern 'mu4e-ask-bookmark)
  (defun mu4e-ask-bookmark (prompt)
    "Ask the user for a bookmark (using PROMPT) as defined in
`mu4e-bookmarks', then return the corresponding query."
    (unless (mu4e-bookmarks) (mu4e-error "No bookmarks defined"))
    (let* ((prompt (mu4e-format "%s" prompt))
           (server-queries (plist-get mu4e~server-props :queries))
           (bmarks
            (mapconcat
             (lambda (bm)
               (let ((bm-server-query (seq-find (lambda (q)
                                                  (string= (plist-get q :query)
                                                           (plist-get bm :query)))
                                                server-queries)))
                 (format "[%s]%s (%s/%s)"
                         (propertize (make-string 1 (plist-get bm :key))
                                     'face 'mu4e-highlight-face)
                         (plist-get bm :name)
                         (plist-get bm-server-query :unread)
                         (plist-get bm-server-query :count))))
             (mu4e-bookmarks)
             ", "))
           (kar (read-char (concat prompt bmarks))))
      (mu4e-get-bookmark-query kar)))
  (add-hook 'mu4e-message-changed-hook #'mu4e~start)
  (add-hook 'mu4e-index-updated-hook #'mu4e~start)
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  ;; add info folder
  (add-to-list 'Info-directory-list "/opt/mu/mu4e/")
  (add-to-list 'mu4e-view-actions '("decrypt inline PGP" . epa-mail-decrypt))
  (add-to-list 'mu4e-view-actions '("browse body" . mu4e-action-view-in-browser)))

(use-package mml-sec
  :config
  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t))

(use-package mm-decode
  :config
  (add-to-list 'mm-inlined-types "application/pgp$")
  (add-to-list 'mm-inline-media-tests
               '("application/pgp$" mm-inline-text identity))
  (add-to-list 'mm-automatic-display "application/pgp$")
  (setq mm-automatic-display
        (remove "application/pgp-signature" mm-automatic-display)))

(use-package taskjuggler-mode)

(use-package windmove
  :bind
  (("C-x <left>"  . 'windmove-left)     ; move to left windnow
   ("C-x <right>" . 'windmove-right)    ; move to right window
   ("C-x <up>"    . 'windmove-up)       ; move to upper window
   ("C-x <down>"  . 'windmove-down)     ; move to downer window
   ))

(use-package nxml
  :mode (("\..*proj$" . nxml-mode)))

(use-package ediff
  ;; https://emacs.stackexchange.com/a/21336/16861
  :config
  (add-hook 'ediff-prepare-buffer-hook #'show-all)
  ;; https://stackoverflow.com/a/29757750
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

(use-package comint
  ;; This is based on
  ;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
  ;; The idea is to store sessions of comint based modes. For example, to enable
  ;; reading/writing of command history in, say, inferior-haskell-mode buffers,
  ;; simply add turn-on-comint-history to inferior-haskell-mode-hook by adding
  ;; it to the :hook directive
  :config
  (defun comint-write-history-on-exit (process event)
    (comint-write-input-ring)
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (insert (format "\nProcess %s %s" process event))))))

  (defun turn-on-comint-history ()
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (setq comint-input-ring-file-name
              (format "~/.emacs.d/inferior-%s-history"
                      (process-name process)))
        (comint-read-input-ring)
        (set-process-sentinel process
                              #'comint-write-history-on-exit))))

  (defun mapc-buffers (fn)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (funcall fn)))
          (buffer-list)))

  (defun comint-write-input-ring-all-buffers ()
    (mapc-buffers 'comint-write-input-ring))

  (add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring))


;;; save history of some modes

(use-package sql
  :after comint
  :config
  (add-hook 'sql-interactive-mode-hook 'turn-on-comint-history))

(use-package python
  :after comint
  :config
  (add-hook 'inferior-python-mode-hook 'turn-on-comint-history))

(use-package esh
  :init
  (require 'em-hist)

  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier.  from
http://www.howardism.org/Technical/Emacs/eshell-fun.html"
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))
      (insert (concat "ls"))
      (eshell-send-input)))
  (defun eshell/c ()
    "clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (defun eshell/x ()
    (delete-window)
    (eshell-save-some-history)
    (eshell/exit))
  (defun eshell-maybe-bol ()
    "I use the following code. It makes C-a go to the beginning of
the command line, unless it is already there, in which case it
goes to the beginning of the line. So if you are at the end of
the command line and want to go to the real beginning of line,
hit C-a twice:"
    (interactive)
    (let ((p (point)))
      (eshell-bol)
      (if (= p (point))
          (beginning-of-line))))
  :config
  (setenv "PAGER" "cat")
  (setq eshell-history-size 1024
        eshell-visual-commands
        ("mtr" "nethogs"  "htop" "ncdu" "nmon" "top" "less" "more"))
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)))
  :bind  (:map
          global-map
          ("C-x !" . 'eshell-here)
          :map
          eshell-command-map
          ("C-a" . eshell-maybe-bol)
          :map
          eshell-mode-map
          ("C-a" . eshell-maybe-bol)))

(use-package ispell
  :no-require t
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

(use-package frame
  :bind (("C-c f" . 'make-frame)))

(use-package paren
  :config (show-paren-mode 1))

(use-package elisp-mode
  :after elisp-slime-nav-mode
  :bind
  (("C-c d"   . 'elisp-disassemble)
   ("C-c m"   . 'elisp-macroexpand)
   ("C-c M"   . 'elisp-macroexpand-all)
   ("C-c C-c" . 'compile-defun))
  :config
  (require 'cl)
  (defun elisp-disassemble (function)
    (interactive (list (function-called-at-point)))
    (disassemble function))

  (defun elisp-pp (sexp)
    (with-output-to-temp-buffer "*Pp Eval Output*"
      (pp sexp)
      (with-current-buffer standard-output
        (emacs-lisp-mode))))

  (defun elisp-macroexpand (form)
    (interactive (list (form-at-point 'sexp)))
    (elisp-pp (macroexpand form)))

  (defun elisp-macroexpand-all (form)
    (interactive (list (form-at-point 'sexp)))
    (elisp-pp (cl-macroexpand-all form)))
  (elisp-slime-nav-mode 1))

(use-package time
  :config
  (setq display-time-format "%F %R %z"
        display-time-mode 1))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package dired-x
  :after dired)

(use-package autorevert
  :config
  (global-auto-revert-mode t))

(use-package simple
  :defer 5
  :bind (("C-w" . 'backward-kill-word)
         ("C-x C-k" . 'kill-region))
  :hook ((before-save . delete-trailing-whitespace))
  :config
  (setq
   column-number-mode t
   auto-fill-mode 1
   async-shell-command-buffer 'new-buffer))

(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package files
  :config
  (setq require-final-newline t))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package menu-bar
  :config
  (menu-bar-mode 0))

(use-package tooltip
  :config
  (tooltip-mode 0))

(use-package files
  :config
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package ox
  :after org
  :config
  (add-to-list
   'org-export-filter-timestamp-functions
   (lambda
     (trans back _comm)
     "Remove <> around time-stamps."
     (pcase back
       (`html
        (replace-regexp-in-string
         "[][]"
         ""
         (replace-regexp-in-string "&[lg]t;" "" trans)))
       ((or `ascii `latex)
        (replace-regexp-in-string "[][<>]" "" trans))))))

;;; packages that are fetched

;;; Libraries
(use-package diminish
  :straight t)

(use-package parse-csv
  :straight t
  :defer t)

(use-package s
  :straight t
  :defer t)

(use-package dash
  :straight t
  :defer t)

(use-package ht
  :straight t
  :defer t)

(use-package org-ql
  :straight t
  :defer t)

;;; packages

(use-package async
  :straight t
  :config
  (require 'smtpmail-async)
  (setq message-send-mail-function 'async-smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package psession
  :straight t
  :config
  (psession-savehist-mode 1)
  (psession-mode 1)
  (psession-autosave-mode 1)
  (bind-key "C-x p s" 'psession-save-winconf)
  (bind-key "C-x p d" 'psession-delete-winconf)
  (bind-key "C-x p j" 'psession-restore-winconf))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(use-package forge
  :straight t
  :after magit)

(use-package git-timemachine
  :straight t
  :after magit)

(use-package switch-window
  :straight t
  :config
  (setq switch-window-threshold 3)
  :bind
  ("C-x o"     . 'switch-window)
  ("C-x 1"     . 'switch-window-then-maximize)
  ("C-x 2"     . 'switch-window-then-split-below)
  ("C-x 3"     . 'switch-window-then-split-right)
  ("C-x 0"     . 'switch-window-then-delete)
  ("C-x 4 d"   . 'switch-window-then-dired)
  ("C-x 4 f"   . 'switch-window-then-find-file)
  ("C-x 4 m"   . 'switch-window-then-compose-mail)
  ("C-x 4 r"   . 'switch-window-then-find-file-read-only)
  ("C-x 4 C-f" . 'switch-window-then-find-file)
  ("C-x 4 C-o" . 'switch-window-then-display-buffer)
  ("C-x 4 0"   . 'switch-window-then-kill-buffer))

(defun my/pdf-bookmark-jump-handler (bmk)
  "Fixes integration w/ bookmark plus"
  (switch-to-buffer (current-buffer))
  (current-buffer))

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.05)
  (advice-add 'pdf-view-bookmark-jump-handler :after 'my/pdf-bookmark-jump-handler))

(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

(use-package web-mode
  :straight t
  :mode "\\.html?\\''")

(use-package hippie-exp
  :straight t
  :bind ("M-/" . hippie-expand)
  :init
  (setf hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-line
          try-complete-lisp-symbol)))

(use-package company
  :straight t
  :demand t
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setf company-idle-delay 0.5
        company-selection-wrap-around t)
  :hook (after-init . global-company-mode))

(use-package helm-company
  :straight t
  :after helm company
  :bind (:map
         company-mode-map ("C-;" . 'helm-company)
         :map
         company-active-map ("C-;" . 'helm-company)))

(use-package helm-org
  :straight t
  :after helm org)

(use-package visual-regexp
  :straight t
  :bind (("C-c v"   . vr/replace)
         ("C-c %"   . vr/query-replace)
         ("<C-m> /" . vr/mc-mark)))


(use-package smartparens
  :straight t
  :demand t
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)

  ;; Enable Smartparens functionality in all buffers.
  (smartparens-global-mode +1)
  (smartparens-strict-mode +1)

  ;; When in Paredit emulation mode, Smartparens binds M-( to wrap the
  ;; following s-expression in round parentheses. By analogy, we
  ;; should bind M-[ to wrap the following s-expression in square
  ;; brackets. However, this breaks escape sequences in the terminal,
  ;; so it may be controversial upstream. We only enable the
  ;; keybinding in windowed mode.
  (when (display-graphic-p)
    (setf (map-elt sp-paredit-bindings "M-[") #'sp-wrap-square))

  ;; Set up keybindings for s-expression navigation and manipulation
  ;; in the style of Paredit.
  (sp-use-paredit-bindings)


  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.
  ;; interfers with e.g. org-mode, enable them specifically in lisp modes instead

  (dolist (key '("M-<up>" "M-<down>"))
    (unbind-key key sp-keymap))

  ;; smartparens
  (require 'smartparens-config)

  (use-feature org-agenda
    :config
    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))

  (defun radian--smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; The following is a really absurdly stupid hack that I can barely
  ;; stand to look at. It needs to be fixed.
  ;;
  ;; Nevertheless, I can't live without the feature it provides (which
  ;; should really come out of the box IMO): when pressing RET after
  ;; inserting a pair, add an extra newline and indent. See
  ;; <https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312>.

  (defun radian--smartparens-pair-setup (mode delim)
    "In major mode MODE, set up DELIM with newline-and-indent."
    (sp-local-pair mode delim nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (dolist (delim '("(" "[" "{"))
    (dolist (mode '(
                    fundamental-mode
                    javascript-mode
                    protobuf-mode
                    text-mode
                    web-mode
                    ))
      (radian--smartparens-pair-setup mode delim)))

  (radian--smartparens-pair-setup #'python-mode "\"\"\"")
  (radian--smartparens-pair-setup #'markdown-mode "```")

  ;; Work around https://github.com/Fuco1/smartparens/issues/1036.
  (when (fboundp 'minibuffer-mode)
    (sp-local-pair #'minibuffer-mode "`" nil :actions nil)
    (sp-local-pair #'minibuffer-mode "'" nil :actions nil))

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; For some reason two C-g's are required to exit out of the
  ;; minibuffer if you've just typed a parenthesis pair. This appears
  ;; to be intentional, but doesn't make a lot of intuitive sense
  ;; since we've disabled highlighting. Kill the problematic
  ;; keybinding. See also
  ;; https://github.com/Fuco1/smartparens/pull/890 which was about a
  ;; similar problem.
  (define-key sp-pair-overlay-keymap (kbd "C-g") nil)

  ;; Quiet some silly messages.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil)))

(use-package eldoc
  :straight t
  :diminish t
  :hook ((c-mode-common
          emacs-lisp-mode
          lisp-interaction-mode
          eval-expression-minibuffer-setup)
         . eldoc-mode))

(use-package elint
  :straight t
  :commands (elint-initialize elint-current-buffer)
  :bind ("C-c e E" . my-elint-current-buffer)
  :preface
  (defun my-elint-current-buffer ()
    (interactive)
    (elint-initialize)
    (elint-current-buffer))
  :config
  (add-to-list 'elint-standard-variables 'current-prefix-arg)
  (add-to-list 'elint-standard-variables 'command-line-args-left)
  (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
  (add-to-list 'elint-standard-variables 'emacs-major-version)
  (add-to-list 'elint-standard-variables 'window-system))

(use-package elisp-depend
  :straight t
  :commands elisp-depend-print-dependencies)

(use-package elisp-docstring-mode
  :straight t
  :commands elisp-docstring-mode)

(use-package elisp-slime-nav
  :straight t
  :diminish
  :hook ((emacs-lisp-mode) . elisp-slime-nav-mode)
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))

(use-package json-mode
  :straight t
  :mode "\\.json\\'")


(use-package info-rename-buffer
  :straight t
  :config (info-rename-buffer-mode 1))

(use-package plantuml-mode
  :straight t
  :mode (("\\.plantuml$" . plantuml-mode)
         ("\\.puml$" . plantuml-mode
          ))
  :config (setq plantuml-jar-path "/home/user/bin/plantuml.jar"
                plantuml-default-exec-mode 'jar))

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package docker-compose-mode
  :straight t
  :mode "docker-compose.*\.yml\\'")

(use-package docker-tramp
  :straight t
  :after tramp
  :defer 5)

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package ledger-mode
  :straight t
  :mode (("\.dat$" . ledger-mode)
         ("\.ledger$" . ledger-mode)
         ("\.timeclock$" . ledger-mode))
  :config
  (add-hook 'ledger-mode-hook (lambda () (company-mode -1)))
  (defun ledger-pcomplete (&optional interactively)
    (interactive "p")
    (completion-at-point)))

(use-package winner
  :straight t
  :config (winner-mode 1))

(use-package helm
  :straight t
  :diminish
  :bind (("C-h a"   . helm-apropos)
         ("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-m" . helm-M-x)
         ("C-x m"   . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x r l" . helm-filtered-bookmarks)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x i"   . helm-imenu)
         ("M-y"     . helm-show-kill-ring)
         ("M-i"     . helm-swoop-without-pre-input)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action)
              ("C-z"   . helm-select-action))
  :config
  (setq helm-ff-transformer-show-only-basename nil
        helm-external-programs-associations '(("zip" . "unzip")
                                              ("mp4" . "smplayer")
                                              ("mkv" . "smplayer")
                                              ("docx" . "libreoffice"))
        helm-completion-style 'emacs
        helm-yank-symbol-first                 t
        helm-move-to-line-cycle-in-source      t
        helm-buffers-fuzzy-matching            t
        helm-ff-auto-update-initial-value      t
        helm-imenu-fuzzy-match                 t
        helm-buffer-max-length                 50
        helm-ff-candidate-number-limit         200
        helm-show-completion-display-function #'helm-show-completion-default-display-function)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-hist-mode-map
                          [remap eshell-previous-matching-input-from-input]
                          'helm-eshell-history))))

(use-package pcomplete-extension
  :straight t)

(use-package pcmpl-args
  :straight t)

(use-package helm-descbinds
  :straight t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-swoop
  :straight t
  :bind (("M-m" . helm-swoop)
	 ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package projectile
  :straight t
  :defer 5
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :diminish
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-mode-line "Projectile"))

(use-package helm-projectile
  :straight t
  :config
  (helm-projectile-on))

(use-package markdown-mode
  :straight t
  :mode (("\.md$" . markdown-mode)))

(use-package elfeed
  :straight t
  :bind (("C-x w" . 'elfeed))
  :config (setq elfeed-search-title-max-width 140
                elfeed-show-entry-switch #'pop-to-buffer))


(use-package elfeed-org
  :straight t
  :after elfeed
  :init (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package async
  :straight t)

(use-package htmlize
  :straight t)

(use-package zenburn-theme
  :straight t
  :defer t)

(use-package solarized-theme
  :straight t
  :defer t)

(use-package gnuplot-mode
  :straight t)

(use-package org-super-agenda
  :straight t
  :config
  (org-super-agenda-mode 1)
  (setq
   org-agenda-custom-commands
   '(("u" "Super view"
      ((agenda "" ((org-super-agenda-groups
                    '((:name "Work Habits"
                             :and (:file-path "data-risk" :habit t)
                             :order 20)
                      (:name "Personal Habits"
                             :and (:file-path "personal" :habit t)
                             :habit t
                             :order 22)
                      (:name "Work day notification"
                             :property "work_reminder"
                             :order 25)
                      (:name "Important"
                             :priority>= "B"
                             :order 0)
                      (:name "Late tasks"
                             :deadline past
                             :scheduled past
                             :order 10)
                      (:name "Regular"
                             :time-grid t
                             :date today
                             :deadline  today
                             :scheduled today
                             :order 30)
                      (:discard (:anything t)))))))
      ((org-overriding-columns-format "%WSJF %ITEM %bv %tc %rr-oe %eff %ALLTAGS"))))))

(use-package calfw
  :straight '(:host github :repo "ebellani/emacs-calfw")
  :config
  (setq cfw:org-overwrite-default-keybinding t
        cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓
        cfw:event-format-overview "%s%e%t"))

(use-package calfw-org
  :straight t
  :after calfw)

(use-package calfw-cal
  :straight t
  :after calfw)

(use-package org-ql
  :straight t
  :after org)

(use-package ox-gfm
  :straight t
  :after ox)

(use-package perspective
  :straight t
  :config
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (setq persp-state-default-file "~/.emacs.d/persp.state"))

(use-package helm-org-rifle
  :straight t
  :bind (("C-c r"   . helm-org-rifle-agenda-files)))

(use-package bufler
  :straight t)

(use-package which-key
  :straight t
  :config (which-key-mode))

(use-package fsharp-mode
  :demand t
  :straight t
  :mode (("\\.fs$" .  fsharp-mode)
	 ("\\.fsx$" .  fsharp-mode))
  :config
  (setq inferior-fsharp-program "dotnet fsi")
  (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history))

(use-package dotnet
  :straight t)

(use-package pyenv-mode
  :straight t
  :bind (:map pyenv-mode-map
              ("C-c C-s" . nil))
  :config (pyenv-mode))

(use-package orgit
  :straight t
  :demand org-plus-contrib)

(use-package org-drill
  :straight t
  :demand org-plus-contrib
  :commands (org-drill))

(use-package gnu-elpa-keyring-update
  :straight t
  :demand t)

(use-package langtool
  :straight t
  :demand t
  :config
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8010))

(use-package editorconfig
  :straight t
  :demand t
  :config
  (editorconfig-mode 1))

(use-package org-msg
  :straight '(:host github :repo "ebellani/org-msg")
  :demand t
  :bind (:map org-msg-edit-mode-map
	      ("C-c RET C-c" . mml-secure-message-sign-encrypt)
              ("C-c RET C-s" . mml-secure-message-sign)
              ("C-c RET t" . message-goto-to)
              ("C-c RET c" . message-goto-cc)
              ("C-c RET b" . message-goto-bcc)
              ("C-c RET o" . org-msg-goto-body)
              ("C-c RET s" . message-goto-subject))
  :config
  (defun myorg/mu4e-compose-org-msg()
    (org-hide-block-all)
    (org-hide-drawer-all))

  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-default-alternatives '((new		. (text html))
				       (reply-to-html	. (text html))
				       (reply-to-text	. (text)))
        org-msg-enforce-css nil
	org-msg-convert-citation t)

  (advice-add 'org-msg-post-setup :after 'myorg/mu4e-compose-org-msg)
  (org-msg-mode))

(use-package helm-mu
  :straight t
  :after mu4e)

(use-package helm-org-contacts
  :straight '(:host github :repo "tmalsburg/helm-org-contacts")
  :after helm-mu
  :config
  (defun helm-contacts (&optional arg)
    (interactive "P")
    (when arg
      (setq helm-org-contacts-cache nil))
    (helm :sources '(helm-source-org-contacts helm-source-mu-contacts)
          :full-frame t
          :candidate-number-limit 500)))

(use-package org-contrib
  :straight t
  :after org
  :config
  (require 'org-contacts)
  (setq org-contacts-files (list  (my/path :agenda "contacts.org")))
  (add-to-list 'org-capture-templates
               `("c" "Contacts" entry (file ,(my/path :agenda "contacts.org"))
                 ,(concat
                   "* %(org-contacts-template-name)\n"
                   ":PROPERTIES:\n"
                   ":EMAIL: %(org-contacts-template-email)\n"
                   ":PHONE:\n"
                   ":BIRTHDAY: %^t\n"
                   ":NOTE:\n"
                   ":END:\n"))))

(use-package proof-general
  :straight t)

(use-package shrface
  :straight t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(defun my/eww-rename-buffer-name ()
  (rename-buffer
   (if-let ((url (eww-current-url))
            (title (plist-get eww-data :title)))
       (format "*eww-%s_%s*" title url)
     (generate-new-buffer "*eww*"))))

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  (add-hook 'eww-after-render-hook #'my/eww-rename-buffer-name)
  :config
  (require 'shrface))

(use-package nov
  :straight t
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :mode (("\\.epub$" . nov-mode))
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

(use-package ox-moderncv
  :straight '(:host gitlab :repo "eduardo-bellani/org-cv")
  :init (require 'ox-moderncv))

(use-package emojify
  :straight t)

(use-package lsp-mode
  :straight t
  :demand   t
  :config
  (add-hook 'fsharp-mode-hook #'lsp))

(use-package lsp-ui
  :straight t
  :demand   t)

(use-package bookmark+
  :straight t
  :demand   t)

(use-package haskell-mode
  :straight t)

(straight-use-package  '(helm-wordnut :host github :repo "emacs-helm/helm-wordnut"))

(use-package slack
  :commands (slack-start)
  :straight t
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  (defun endless/slack-message-embed-mention ()
    (interactive)
    (call-interactively #'slack-message-embed-mention)
    (insert " "))
  (defun my/thumbs-up ()
    (interactive)
    (insert ":+1:"))
  ;; from http://endlessparentheses.com/keep-your-slack-distractions-under-control-with-emacs.html
  :bind (:map
         global-map
         ("C-c C-l j" . #'slack-select-rooms)
         :map
         slack-thread-message-buffer-mode-map
         ("C-=" . my/thumbs-up)
         ("@"   . endless/slack-message-embed-mention)
         :map
         slack-mode-map
	 ("C-=" . my/thumbs-up)
         ("@"   . endless/slack-message-embed-mention)
         (">"   . slack-thread-show-or-create))
  :config
  (setq slack-thread-also-send-to-room nil)
  (setq lui-time-stamp-format "%F [%R] %Z")
  (run-with-timer 0 (* 25 60)
                  (lambda ()
                    (slack-ws--reconnect (oref slack-current-team :id) t)
                    (slack-im-list-update))))

(use-package helm-slack
  :straight '(:host github :repo "yuya373/helm-slack")
  :after (slack))

(use-package alert
  :straight t
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(put 'scroll-left 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
