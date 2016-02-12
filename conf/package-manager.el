(require 'package)

(defvar my-repositories '(("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/"))
  "Repositories that I'm using to retrieve packages.")

(dolist (rep my-repositories)
  (add-to-list 'package-archives
               rep))

(package-initialize)

;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(defvar my-packages
  '(slime
    symon
    highlight-parentheses
    highlight-symbol
    paredit
    scala-mode
    auctex
    rainbow-delimiters
    ido-ubiquitous
    haml-mode
    smex
    cider
    undo-tree
    haskell-mode
    yasnippet
    clojure-mode
    zenburn-theme
    ;; tron-theme
    rinari
    magit
    rvm
    yari
    markdown-mode
    web-mode
    yaml-mode
    geiser
    ;; racket-mode
    noctilux-theme
    edit-server
    smartparens
    fsharp-mode
    ;; ocaml
    tuareg
    ocp-indent
    merlin
    utop
    ;; auto-complete
    company
    ;; accounting
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
