;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs

;; common lisp compatibility
(require 'cl)

(defvar *my-default-lib* "~/.emacs.d/lib"
  "Vendor libraries that cannot be installed via the package system")
(defvar *my-conf* "~/.emacs.d/conf/"
  "My configurations")
(defvar *elpa-path* "~/.emacs.d/elpa/"
  "Elpa packages")
(defvar *domain-custom* "~/emacs-domain-custom/"
  "This is supposed to be where files that are specific to a
  domin should reside. For example, history files. The point of
  this is to allow a single emacs configuration to be shared
  easily by multiple VM domains.")
(add-to-list 'load-path *my-default-lib*)
(add-to-list 'load-path *my-conf*)

(load "backup.el")
(load "package-manager.el")
(load "elisp.el")
(load "org-conf.el")
(load "misc.el")
(load "keybindings.el")
(load "ido-conf.el")
(load "autocompletion.el")
(load "eshell-conf.el")
(load "smex-conf.el")
(load "spell-checking.el")
(load "python-conf.el")
(load "lisp-conf.el")
(load "scheme-conf.el")
(load "js-conf.el")
(load "w3m-conf.el")
(load "sql-conf.el")
(load "clojure.el")
(load "ocaml.el")

(setq custom-file (concat *domain-custom* "custom.el"))
(load custom-file 'noerror)
