;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs

;;; path setup

;; prepare a folder for custom libraries
(defvar *my-default-lib* "~/.emacs.d/lib"
  "Vendor libraries that cannot be installed via the package system")
(add-to-list 'load-path *my-default-lib*)

;; add the custom file inside the emacs folder
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; old libraries 

;; (load "package-manager.el")
;; (load "web-conf.el")
;; (load "backup.el")
;; (load "elisp.el")
;; (load "org-conf.el")
;; (load "misc.el")
;; (load "keybindings.el")
;; (load "eshell-conf.el")
;; (load "helm-conf.el")
;; (load "autocompletion.el")
;; (load "spell-checking.el")
;; (load "python-conf.el")
;; (load "lisp-conf.el")
;; (load "scheme-conf.el")
;; (load "js-conf.el")
;; (load "sql-conf.el")
;; (load "ledger-conf.el")
;; (load "mu4e-conf.el")
;; (load "comint-conf.el")
;; (load "calendar-conf.el")
;; (load "taskjuggler-conf.el")
;; (load custom-file 'noerror)
;; (put 'upcase-region 'disabled nil)
;; (put 'scroll-left 'disabled nil)

;;; things that I don't know how to do with use-package

;; set menu as M
(define-key key-translation-map (kbd "<menu>") (kbd "ESC"))

(setq inhibit-startup-screen t
      pop-up-frames nil
      standard-indent 2)

(setq-default indent-tabs-mode nil)

;;; use-package config way

;; install use-package
;; see http://cachestocaches.com/2015/8/getting-started-use-package/
;; and http://cestlaz.github.io/posts/using-emacs-1-setup/
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; packages that come with emacs

(setq use-package-always-ensure nil)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p 1)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package autorevert
  :config
  (global-auto-revert-mode t))

(use-package simple
  :config
  (column-number-mode t))

(use-package hl-line
  :config
  (global-hl-line-mode t))  

(use-package files
  :config
  (setq require-final-newline t))

(use-package scroll-bar
  :config
  (set-scroll-bar-mode nil))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package menu-bar
  :config
  (menu-bar-mode 0))

(use-package tooltip
  :config
  (tooltip-mode 0))


;;; external packages, need to ensure that they are downloaded

(setq use-package-always-ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package psession
  :config
  (setq psession-mode t)
  (global-undo-tree-mode)
  (setq psession-object-to-save-alist
 '((ioccur-history                       . "ioccur-history.el")
          (extended-command-history      . "extended-command-history.el")
          (helm-external-command-history . "helm-external-command-history.el")
          (helm-surfraw-engines-history  . "helm-surfraw-engines-history.el")
          (psession--save-buffers-alist  . "psession-save-buffers-alist.el")
          (helm-ff-history               . "helm-ff-history.el")
          (helm-grep-history             . "helm-grep-history.el")
          (register-alist                . "register-alist.el")
          (psession--winconf-alist       . "psession-winconf-alist.el"))))

(use-package magit)

(use-package switch-window
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
