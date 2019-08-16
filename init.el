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

;;; path setup

;; prepare a folder for custom libraries
(defvar *my-default-lib* "~/.emacs.d/lib"
  "Vendor libraries that cannot be installed via the package system")

(add-to-list 'load-path *my-default-lib*)

(defvar org-refile-file-path "~/.emacs.d/refile.org"
  "A place to hold temporary refile information.")

(defcustom shared-agenda-file "~/.emacs.d/shared-agenda.org"
  "This is the shared agenda (in the 'cloud'). Usually this means
  something shared with google calendar."
  :type 'file)

(defvar shared-capture-key "g"
  "Key to use to capture shared entries")

;; add the custom file inside the emacs folder
(defvar custom-file-path "~/.emacs.d/custom.el"
  "Place where I store my local customizations. This file is not ")
(if (file-readable-p custom-file-path)
    (progn
      (setq custom-file custom-file-path)
      (load custom-file))
  (warn "Custom file not found at expected path %s" custom-file-path))

;; font family & size

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 100)

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
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(unless package--initialized (package-initialize t))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; packages that come with emacs

(setq use-package-always-ensure nil)

(use-package browse-url
  :config
  (setq browse-url-browser-function 'browse-url-firefox))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(use-package server
  :config
  (server-start))

(defun filter-bad-contacts (contact)
  "This is used to filter the bad contacts that mu4e is
accumulating."
  (let ((name (or (plist-get contact :name) ""))
        (mail (plist-get contact :mail)))
    (if (or (and (string= "Mateus Costa" name)
                 (string= "yurialbuquerque@brickabode.com" mail))
            (string-match-p "bickabode" mail)
            (string-match-p "brickabode.con" mail))
        nil
      contact)))

(use-package mu4e
  :load-path "/opt/mu/mu4e/"
  :config
  (setq)
  (require 'mu4e-contrib)
  ;; general config
  (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-encrypt)
  (setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc gmail"
        mu4e-contact-rewrite-function 'filter-bad-contacts
        ;;  "html2text -utf8 -width 72" ?
        ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
        ;; stop UID errors
        mu4e-change-filenames-when-moving t
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-update-interval 120
        mu4e-headers-auto-update t
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
        mu4e-view-use-gnus t)
  ;; add info folder
  (add-to-list 'Info-directory-list "/opt/mu/mu4e/")
  (add-to-list 'mu4e-view-actions '("decrypt inline PGP" . epa-mail-decrypt))
  (add-to-list 'mu4e-view-actions '("browse body" . mu4e-action-view-in-browser)))


(use-package smtpmail-async
  ;; this is fixed for gmail for now. Mu4e contexts could be used to set
  ;; multiple values.
  :after mu4e
  :config
  (setq message-send-mail-function 'async-smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t))

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

(use-package org
  :bind (("C-c l" . 'org-store-link)
         ("C-c c" . 'org-capture)
         ("C-c a" . 'org-agenda)
         ("C-c b" . 'org-iswitchb))
  :init
  (setq org-export-backends '(ascii html icalendar latex md odt org))
  :config
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-capture-templates
        `((,shared-capture-key "Shared Calendar " entry
                               (file ,shared-agenda-file)
                               "* %?
%^T")
          ("t" "todo" entry
           (file ,org-refile-file-path)
           "* TODO %?"))
        org-tag-alist '((:startgroup)
                        ("noexport" . ?n)
                        ("export" . ?e)
                        (:endgroup))
        org-refile-targets
        '((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9))
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
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
        org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")
        org-duration-format '((special . h:mm))
        org-file-apps
        '((auto-mode . emacs)
          ("\\.x?html?\\'" . "x-www-browser %s"))
        org-goto-interface 'outline-path-completion
        ;; allows multiple agenda views to coexist
        org-agenda-sticky t
        org-agenda-span 'day
        org-latex-pdf-process (list "latexmk -f -pdf %f"))
  (add-to-list 'org-structure-template-alist '("n" "#+NAME: ?") )
  ;; format timestamps. See
  ;; http://endlessparentheses.com/better-time-stamps-in-org-export.html
  ;; get images to reload after execution. Useful for things such as
  ;; gnuplot. See https://emacs.stackexchange.com/q/3302
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq-default org-display-custom-times t)
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
     (org    . t))))

(add-to-list 'load-path (concat *my-default-lib* "/org-ql"))
;; org-ql
(require 'org-ql)
(require 'org-ql-agenda)

(use-package ox
  :after org
  :config
  (add-to-list 'org-export-filter-timestamp-functions
               (lambda
                 (trans back _comm)
                 "Remove <> around time-stamps."
                 (pcase back
                   (`html
                    (replace-regexp-in-string "[][]"
                                              ""
                                              (replace-regexp-in-string "&[lg]t;" "" trans)))
                   ((or `ascii `latex)
                    (replace-regexp-in-string "[][<>]" "" trans))))))

(use-package ediff
  ;; https://emacs.stackexchange.com/a/21336/16861
  :config (add-hook 'ediff-prepare-buffer-hook #'show-all))

(use-package comint
  ;; This is based on
  ;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
  ;; The idea is to store sessions of comint based modes. For example, to enable
  ;; reading/writing of command history in, say, inferior-haskell-mode buffers,
  ;; simply add turn-on-comint-history to inferior-haskell-mode-hook by adding
  ;; it to the :hook directive
  :hook (inferior-python-mode sql-interactive-mode)
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

(use-package eshell
  :init
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
  (setq eshell-visual-commands
        '("htop" "ncdu" "nmon" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"))
  :bind  (("C-a"   . 'eshell-maybe-bol)
          ("C-x !" . 'eshell-here)))

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
  (column-number-mode t)
  (auto-fill-mode 1))

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

;;; external packages, need to ensure that they are downloaded

(setq use-package-always-ensure t)

;;; Libraries

(use-package diminish :demand t)

(use-package s
  :defer t)

(use-package shadchen
  :defer t)

(use-package dash
  :defer t)

(use-package ht
  :defer t)

;;; packages

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package psession
  :init
  (setq psession-object-to-save-alist
        '((ioccur-history                . "ioccur-history.el")
          (extended-command-history      . "extended-command-history.el")
          (helm-external-command-history . "helm-external-command-history.el")
          (helm-surfraw-engines-history  . "helm-surfraw-engines-history.el")
          (psession--save-buffers-alist  . "psession-save-buffers-alist.el")
          (helm-ff-history               . "helm-ff-history.el")
          (helm-grep-history             . "helm-grep-history.el")
          (register-alist                . "register-alist.el")
          (psession--winconf-alist       . "psession-winconf-alist.el")))
  :config
  (psession-mode 1))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit)

(use-package git-timemachine
  :after magit)

(use-package switch-window
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
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package web-mode
  :mode "\\.html?\\'")

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :init
  (setf hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-line
          try-complete-lisp-symbol)))

(use-package company
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setf company-idle-delay 0
        company-selection-wrap-around t)
  (global-company-mode t))

(use-package helm-company
  :after helm company
  :bind (:map
         company-mode-map ("C-;" . 'helm-company)
         :map
         company-active-map ("C-;" . 'helm-company)))

(use-package paredit
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

(use-package clojure-mode)

(use-package cider)

(use-package visual-regexp
  :bind (("C-c r"   . vr/replace)
         ("C-c %"   . vr/query-replace)
         ("<C-m> /" . vr/mc-mark)))


(use-package slime
  :commands slime
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy)))

(use-package smartparens
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
  :diminish
  :hook ((c-mode-common
          emacs-lisp-mode
          lisp-interaction-mode
          eval-expression-minibuffer-setup)
         . eldoc-mode))

(use-package elint
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
  :commands elisp-depend-print-dependencies)

(use-package elisp-docstring-mode
  :commands elisp-docstring-mode)

(use-package elisp-slime-nav
  :diminish
  :hook ((emacs-lisp-mode) . elisp-slime-nav-mode)
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))

;; (use-package phi-search
;;   :defer 5)

;; (use-package phi-search-mc
;;   :after (phi-search multiple-cursors)
;;   :config
;;   (phi-search-mc/setup-keys)
;;   (add-hook 'isearch-mode-mode #'phi-search-from-isearch-mc/setup-keys))

;; (use-package multiple-cursors
;;   :after phi-search
;;   :defer 5
;;   ;; - Sometimes you end up with cursors outside of your view. You can scroll
;;   ;;   the screen to center on each cursor with `C-v` and `M-v`.
;;   ;;
;;   ;; - If you get out of multiple-cursors-mode and yank - it will yank only
;;   ;;   from the kill-ring of main cursor. To yank from the kill-rings of every
;;   ;;   cursor use yank-rectangle, normally found at C-x r y.
;;   :bind (("<C-m> ^"     . mc/edit-beginnings-of-lines)
;;          ("<C-m> `"     . mc/edit-beginnings-of-lines)
;;          ("<C-m> $"     . mc/edit-ends-of-lines)
;;          ("<C-m> '"     . mc/edit-ends-of-lines)
;;          ("<C-m> R"     . mc/reverse-regions)
;;          ("<C-m> S"     . mc/sort-regions)
;;          ("<C-m> W"     . mc/mark-all-words-like-this)
;;          ("<C-m> Y"     . mc/mark-all-symbols-like-this)
;;          ("<C-m> a"     . mc/mark-all-like-this-dwim)
;;          ("<C-m> c"     . mc/mark-all-dwim)
;;          ("<C-m> l"     . mc/insert-letters)
;;          ("<C-m> n"     . mc/insert-numbers)
;;          ("<C-m> r"     . mc/mark-all-in-region)
;;          ("<C-m> s"     . set-rectangular-region-anchor)
;;          ("<C-m> %"     . mc/mark-all-in-region-regexp)
;;          ("<C-m> t"     . mc/mark-sgml-tag-pair)
;;          ("<C-m> w"     . mc/mark-next-like-this-word)
;;          ("<C-m> x"     . mc/mark-more-like-this-extended)
;;          ("<C-m> y"     . mc/mark-next-like-this-symbol)
;;          ("<C-m> C-x"   . reactivate-mark)
;;          ("<C-m> C-SPC" . mc/mark-pop)
;;          ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
;;          ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
;;          ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
;;          ("<C-m> ["     . mc/vertical-align-with-space)
;;          ("<C-m> {"     . mc/vertical-align)
;;          ("S-<down-mouse-1>")
;;          ("S-<mouse-1>" . mc/add-cursor-on-click))
;;   :bind (:map selected-keymap
;;               ("c"   . mc/edit-lines)
;;               ("."   . mc/mark-next-like-this)
;;               ("<"   . mc/unmark-next-like-this)
;;               ("C->" . mc/skip-to-next-like-this)
;;               (","   . mc/mark-previous-like-this)
;;               (">"   . mc/unmark-previous-like-this)
;;               ("C-<" . mc/skip-to-previous-like-this)
;;               ("y"   . mc/mark-next-symbol-like-this)
;;               ("Y"   . mc/mark-previous-symbol-like-this)
;;               ("w"   . mc/mark-next-word-like-this)
;;               ("W"   . mc/mark-previous-word-like-this))
;;   :preface
;;   (defun reactivate-mark ()
;;     (interactive)
;;     (activate-mark)))

(use-package json-mode
  :mode "\\.json\\'")

(use-package helpful
  :bind (("C-h C" . helpful-command)
         ("C-h M" . helpful-macro)
         ("C-h f" . helpful-callable)
         ("C-c C-d" . helpful-at-point)
         ("C-h v" . helpful-variable)
         ("C-h F" . helpful-function)
         ("C-h k" . helpful-key)))

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :config (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))

(use-package docker
  :bind ("C-c d" . docker))

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(use-package docker-tramp
  :after tramp
  :defer 5)

(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package ledger-mode
  :mode (("\.dat$" . ledger-mode)
         ("\.ledger$" . ledger-mode))
  :config
  (add-hook 'ledger-mode-hook (lambda () (company-mode -1)))
  (defun ledger-pcomplete (&optional interactively)
    (interactive "p")
    (completion-at-point)))

(use-package winner
  :config (winner-mode 1))

(use-package helm
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
        helm-yank-symbol-first                 t
        helm-move-to-line-cycle-in-source      t
        helm-buffers-fuzzy-matching            t
        helm-ff-auto-update-initial-value      t
        helm-imenu-fuzzy-match                 t
        helm-buffer-max-length                 nil
        ;; the following would enable a separate frame.
        ;; helm-display-function                  'helm-display-buffer-in-own-frame
        ;; helm-display-buffer-reuse-frame        t
        ;; helm-use-undecorated-frame-option      t
        )
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))))

(use-package pcomplete-extension)

(use-package helm-company
  :after helm company
  :bind ((:map company-mode-map ("C-;" . 'helm-company))
         (:map company-active-map ("C-;" . 'helm-company))))

(use-package helm-flx
  :after helm
  :config (setq helm-flx-for-helm-find-files t ;; t by default
                helm-flx-for-helm-locate t) ;; nil by default
 )

(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

(use-package helm-swoop
  :bind (("M-m" . helm-swoop)
	 ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package helm-wordnet
  :after helm)

(use-package projectile
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
  :config
  (helm-projectile-on))

(use-package elscreen
  :config
  (setq elscreen-tab-display-control nil  ; hide control tab at the left side
        elscreen-tab-display-kill-screen nil ; hide kill button
        elscreen-display-tab t)
  (elscreen-start))

(use-package helm-mu
  :after mu4e)

(use-package powershell
  :mode (("\.ps1$" . powershell-mode)))

(use-package markdown-mode
  :mode (("\.md$" . markdown-mode)))

(use-package elfeed
  :bind (("C-x w" . 'elfeed)))

(use-package elfeed-org
  :after elfeed
  :init (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package org-pomodoro
  :after org)

(use-package async)

(use-package fsharp-mode)

(use-package htmlize)

(use-package zenburn-theme :ensure t :defer t)

(use-package solarized-theme :ensure t :defer t)

(use-package gnuplot-mode)

(use-package org-super-agenda
  :ensure t
  :demand t
  :config
  (org-super-agenda-mode 1)
  :preface
  (add-to-list
   'org-agenda-custom-commands
   '("u" "Super view"
     ((agenda "" ((org-super-agenda-groups
                   '((:order-multi
                      (0 (:name "Today"
                                :time-grid t
                                :date today
                                :deadline  today
                                :scheduled today)
                         (:name "Late"
                                :deadline  past
                                :scheduled past)))))))
      (todo "" ((org-agenda-overriding-header "Overall")
                (org-super-agenda-groups
                 '((:name "Personal"
                          :tag "personal"
                          :order 2)
                   (:name "Brick Abode"
                          :tag "brickabode"
                          :order 3)
                   (:name "Green Eye"
                          :tag "green_eye"
                          :order 4)))))))))

(use-package org-gcal
  :ensure t
  ;; need to custom some things:
  ;; see https://github.com/kidd/org-gcal.el
  ;; (setq org-gcal-client-id "oauth 2.0 client ID"
  ;;       org-gcal-client-secret "client secret"
  ;;       org-gcal-file-alist '(("zamansky@gmail.com" .  "~/Dropbox/orgfiles/gcal.org")))
  :config
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch)))
  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (when (equal (plist-get org-capture-plist :key)
                           shared-capture-key)
                (org-gcal-post-at-point)))))

(use-package ox-gfm
  :after ox)

(put 'scroll-left 'disabled nil)
