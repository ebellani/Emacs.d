;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; https://github.com/cqql/dotfiles/blob/master/home/.emacs.d/init.org


(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

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


;;; path setup

;; prepare a folder for custom libraries
(defvar *my-default-lib* "~/.emacs.d/lib"
  "Vendor libraries that cannot be installed via the package system")

(add-to-list 'load-path *my-default-lib*)

(defvar org-refile-file-path "~/.emacs.d/refile.org"
  "A place to hold temporary refile information.")

(defcustom filter-bad-contacts
  #'identity
  "This is used to filter the bad contacts that mu4e is
accumulating.")

(defcustom srs-deck "~/.emacs.d/deck.org"
  "Location of my speaced repetition system (SRS) deck for
  refiling purposes")

(defcustom meetings "~/.emacs.d/meetings.org"
  "Location of my meetings ile for refiling purposes")

;; add the custom file inside the emacs folder
(defvar custom-file-path "~/.emacs.d/custom.el"
  "Place where I store my local customizations. This file is not ")
(if (file-readable-p custom-file-path)
    (progn
      (setq custom-file custom-file-path)
      (load custom-file))
  (warn "Custom file not found at expected path %s" custom-file-path))

(setq *my-font-size* 90)

;;; font family & size

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height *my-font-size*)

;;; things that I don't know how to do with use-package

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t
      pop-up-frames nil
      standard-indent 2)

(setq-default indent-tabs-mode nil
              fill-column 80)

;; Maps swaps [ for ( and vice versa. I use parens much more than square
;; brackets.
(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

(define-key key-translation-map (kbd "<menu>") (kbd "ESC"))

;;; use-package config way

;; install use-package
;; see http://cachestocaches.com/2015/8/getting-started-use-package/
;; and http://cestlaz.github.io/posts/using-emacs-1-setup/
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"       . "https://orgmode.org/elpa/") t)

(unless package--initialized (package-initialize t))

(eval-when-compile
  (require 'use-package))

;;; packages that come with emacs
(use-package linum
  :config
  (eval-after-load "linum"
    '(set-face-attribute 'linum nil :height *my-font-size*)))

(use-package browse-url
  :config
  (setq browse-url-browser-function 'browse-url-chrome))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(use-package minibuffer
  :config
  (setq completion-styles '(flex))
  (scroll-bar-mode 0))

(use-package mu4e
  :load-path "/opt/mu/mu4e/"
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
        )
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
  ;; add info folder
  (add-to-list 'Info-directory-list "/opt/mu/mu4e/")
  (add-to-list 'mu4e-view-actions '("decrypt inline PGP" . epa-mail-decrypt))
  (add-to-list 'mu4e-view-actions '("browse body" . mu4e-action-view-in-browser)))

(use-package org-mu4e)

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

(use-package jl-encrypt)

(use-package taskjuggler-mode)

(use-package windmove
  :bind
  (("C-x <left>"  . 'windmove-left)     ; move to left windnow
   ("C-x <right>" . 'windmove-right)    ; move to right window
   ("C-x <up>"    . 'windmove-up)       ; move to upper window
   ("C-x <down>"  . 'windmove-down)     ; move to downer window
   ))

(use-package epa
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

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
        '("mtr" "nethogs"  "htop" "ncdu" "nmon"
          "vi" "screen" "top" "less" "more" "lynx"
          "ncftp" "pine" "tin" "trn" "elm"))
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

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p 1)
  (setq uniquify-ignore-buffers-re "^\\*"))

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

(use-package org
  :bind (("C-c l" . 'org-store-link)
         ("C-c c" . 'org-capture)
         ("C-c a" . 'org-agenda)
         ("C-c b" . 'org-iswitchb))
  ;; https://github.com/raxod502/straight.el/issues/270#issuecomment-380262852
  :straight org-plus-contrib
  :preface
  (setq org-export-backends '(md gfm beamer ascii taskjuggler html latex odt org))
  :config
  (require 'org-tempo)
  (defun replace-in-string (what with in)
    (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

  (defun org-html--format-image (source attributes info)
    (progn
      (setq source (replace-in-string "%20" " " source))
      (format "<img src=\"data:image/%s;base64,%s\"%s />"
              (or (file-name-extension source) "")
              (base64-encode-string
               (with-temp-buffer
                 (insert-file-contents-literally source)
                 (buffer-string)))
              (file-name-nondirectory source))))
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-tag-alist '((:startgroup)
                        ("noexport" . ?n)
                        ("export" . ?e)
                        (:endgroup))
        org-refile-targets
        '((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 1)
          (srs-deck :maxlevel . 2)
          (meetings  :maxlevel . 2))
        org-capture-templates
        '(("t" "todo" entry
           (file "~/.emacs.d/refile.org")
           "* TODO %?
   SCHEDULED: %t
   :LOGBOOK:
   - State \"TODO\"       from \"\"  %U  \\\\
   :END:
")
          ("r" "reunião" entry
           (file "~/.emacs.d/refile.org")
"* %u %?
** Contexto
** Objetivo
** Agenda
** Ata")
          ("m" "meeting" entry
           (file "~/.emacs.d/refile.org")
"* %u %?
** Context
** Goal
** Agenda
** Minutes")
          ("1" "1-1 meeting" entry
           (file "~/.emacs.d/refile.org")
           "* %u %?
")
          ("c" "SRS card" entry
           (file "~/.emacs.d/refile.org")
           "* Item    :drill:
   %?
** Back
"))
        org-todo-keywords
        '((sequence "TODO(t@/!)" "|" "DONE(d@/!)")
          (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))
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
        org-agenda-include-diary t
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t
        ;; allows multiple agenda views to coexist
        org-agenda-sticky t
        org-agenda-span 'day
        org-latex-pdf-process (list "latexmk -f -pdf %f"))
  ;; format timestamps. See
  ;; http://endlessparentheses.com/better-time-stamps-in-org-export.html
  ;; get images to reload after execution. Useful for things such as
  ;; gnuplot. See https://emacs.stackexchange.com/q/3302
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
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
     (ledger . t)
     (org    . t)))
  (require 'ol-git-link)
  (defun org-set-as-habit ()
    (interactive)
    (org-set-property "STYLE" "habit")))

(use-package org-tempo
  :after org)

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

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; fix space next page problem. No idea why
  ;; (setq pdf-view-have-image-mode-pixel-vscroll nil)
  (pdf-tools-install))

(use-package web-mode
  :straight t
  :mode "\\.html?\\'\\|\\.fsproj$\\'")

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
  :ensure t
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setf company-idle-delay 0
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

(use-package paredit
  :straight t
  :diminish
  :hook ((lisp-mode emacs-lisp-mode clojure-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("C-c ( n"   . paredit-add-to-next-list)
              ("C-c ( p"   . paredit-add-to-previous-list)
              ("C-c ( j"   . paredit-join-with-next-list)
              ("C-c ( J"   . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :config
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(use-package visual-regexp
  :straight t
  :bind (("C-c v"   . vr/replace)
         ("C-c %"   . vr/query-replace)
         ("<C-m> /" . vr/mc-mark)))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (bind-keys :map smartparens-mode-map
             ("C-M-f" . sp-forward-sexp)
             ("C-M-S-f" . sp-next-sexp)
             ("C-M-b" . sp-backward-sexp)
             ("C-M-S-b" . sp-previous-sexp)
             ("C-M-n" . sp-down-sexp)
             ("C-M-S-n" . sp-backward-down-sexp)
             ("C-M-p" . sp-up-sexp)
             ("C-M-S-p" . sp-backward-up-sexp)
             ("C-M-a" . sp-beginning-of-sexp)
             ("C-M-e" . sp-end-of-sexp)
             ("C-M-k" . sp-kill-sexp)
             ("C-M-S-k" . sp-backward-kill-sexp)
             ("C-M-w" . sp-copy-sexp)
             ("C-M-t" . sp-transpose-sexp)
             ("C-M-h" . sp-backward-slurp-sexp)
             ("C-M-S-h" . sp-backward-barf-sexp)
             ("C-M-l" . sp-forward-slurp-sexp)
             ("C-M-S-l" . sp-forward-barf-sexp)
             ("C-M-j" . sp-splice-sexp)
             ("C-M-S-j" . sp-raise-sexp))
  (smartparens-global-mode t)
  (smartparens-strict-mode t)
  (show-smartparens-global-mode t)
  ;; We write it the verbose way instead of with sp-with-modes because
  ;; use-package does not properly expand the macro somehow during compilation
  (sp-local-pair sp--html-modes "{{" "}}")
  (sp-local-pair sp--html-modes "{%" "%}")
  (sp-local-pair sp--html-modes "{#" "#}"))

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

(use-package helpful
  :straight t
  :bind (("C-h C" . helpful-command)
         ("C-h M" . helpful-macro)
         ("C-h f" . helpful-callable)
         ("C-c C-d" . helpful-at-point)
         ("C-h v" . helpful-variable)
         ("C-h F" . helpful-function)
         ("C-h k" . helpful-key)))

(use-package info-rename-buffer
  :straight t
  :config (info-rename-buffer-mode 1))

(use-package plantuml-mode
  :straight t
  :mode (("\\.plantuml$" . plantuml-mode)
         ("\\.puml$" . plantuml-mode
          ))
  :config (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
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
        helm-external-programs-associations '(("zip" . "unzip") ("mp4" . "smplayer") ("mkv" . "smplayer"))
        helm-completion-style 'emacs
        helm-yank-symbol-first                 t
        helm-move-to-line-cycle-in-source      t
        helm-buffers-fuzzy-matching            t
        helm-ff-auto-update-initial-value      t
        helm-imenu-fuzzy-match                 t
        helm-buffer-max-length                 nil
        helm-show-completion-display-function #'helm-show-completion-default-display-function
        ;; the following would enable a separate frame.
        ;; helm-display-function                  'helm-display-buffer-in-own-frame
        ;; helm-display-buffer-reuse-frame        t
        ;; helm-use-undecorated-frame-option      t
        helm-display-buffer-width  120)
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
  :config (setq elfeed-search-title-max-width 140))

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
                    '((:name "Important"
                             :priority>= "B")
                      (:name "Late tasks"
                             :deadline  past
                             :scheduled past)
                      (:name "Day's tasks"
                             :time-grid t
                             :date today
                             :deadline  today
                             :scheduled today)
                      (:discard (:anything t)))))))))))

(use-package calfw
  :straight t

  :config
  (setq cfw:org-overwrite-default-keybinding t))

(use-package calfw-org
  :straight t
  :after calfw)

(use-package org-ql
  :straight t
  :after org)

(use-package ox-gfm
  :straight t
  :after ox)

(use-package helm-mu
  :straight t
  :after mu4e)

(use-package w3m
  :straight t
  :init (setq w3m-key-binding 'info))

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

(use-package helm-bufler
  :straight t)

(use-package which-key
  :straight t
  :config (which-key-mode))

(use-package eglot
  :straight t
  :after company)

(use-package fsharp-mode
  :straight t
  :after compayn
  :config
  (require 'eglot-fsharp)
  (add-hook 'inferior-fsharp-mode-hook'turn-on-comint-history))

(use-package dotnet
  :straight t)

(use-package pyenv-mode
  :straight t
  :bind (:map pyenv-mode-map
              ("C-c C-s" . nil)))

(use-package orgit
  :straight t
  :ensure org-plus-contrib)

(use-package org-drill
  :straight t
  :ensure org-plus-contrib
  :commands (org-drill))

(use-package gnu-elpa-keyring-update
  :straight t
  :ensure t)

(straight-use-package  '(helm-wordnut :host github :repo "emacs-helm/helm-wordnut"))

(put 'scroll-left 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'downcase-region 'disabled nil)
