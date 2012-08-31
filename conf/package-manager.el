(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
	     '("ELPA"      . "http://tromey.com/elpa/"))

(package-initialize)

;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(defvar my-packages '(ac-slime
                      highlight-parentheses
                      highlight-symbol
                      paredit
                      scala-mode
                      auctex
                      rainbow-delimiters
                      ido-ubiquitous
                      javadoc-help
                      smex
                      nrepl
		      undo-tree
		      haskell-mode
                      yasnippet
                      yasnippet-bundle
                      clojure-mode
                      zenburn-theme)
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
