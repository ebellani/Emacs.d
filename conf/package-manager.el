(require 'package)

(defvar my-repositories '(("melpa" . "https://melpa.org/packages/")
                          ("org" . "http://orgmode.org/elpa/"))
  "Repositories that I'm using to retrieve packages.")

(dolist (rep my-repositories)
  (add-to-list 'package-archives
               rep))

(package-initialize)

;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(defvar my-packages
  '(;;; lisp
    paredit
    slime
    rainbow-delimiters
    ;;; latex
    auctex
    ;;; containers
    dockerfile-mode
    docker-tramp
    ;;; python
    pony-mode
    virtualenvwrapper
    ;;; clojure
    clojure-mode
    cider
    ;;; git
    magit
    ;;; web
    w3m ; a browser
    markdown-mode
    web-mode
    yaml-mode
    haml-mode
    ;;; misc
    evil
    legalese
    htmlize
    company
    edit-server
    smartparens
    undo-tree
    yasnippet
    projectile
    helm
    helm-projectile
    helm-swoop
    psession
    flycheck
    ;;; racket & scheme
    geiser
    ;;; better help
    help+
    help-fns+
    help-mode+
    ;;; ocaml
    tuareg
    ocp-indent
    merlin
    utop
    ;;; org
    org-plus-contrib
    ;;; accounting
    ledger-mode)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p))
        do (progn (message "%s" p) (return nil))
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
