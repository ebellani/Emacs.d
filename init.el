;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs
(defvar *my-default-lib* "~/.emacs.d/lib"
  "Vendor libraries that cannot be installed via the package system")
(defvar *my-conf* "~/.emacs.d/conf/"
  "My configurations")
(defvar *domain-custom* "~/emacs-domain-custom/"
  "This is supposed to be where files that are specific to a
  domin should reside. For example, history files. The point of
  this is to allow a single emacs configuration to be shared
  easily by multiple VM domains.")
(add-to-list 'load-path *my-default-lib*)
(add-to-list 'load-path *my-conf*)

(load "package-manager.el")
(load "web-conf.el")
(load "backup.el")
(load "elisp.el")
(load "org-conf.el")
(load "misc.el")
(load "keybindings.el")
(load "eshell-conf.el")
(load "helm-conf.el")
(load "autocompletion.el")
(load "spell-checking.el")
(load "python-conf.el")
(load "lisp-conf.el")
(load "scheme-conf.el")
(load "js-conf.el")
(load "sql-conf.el")
(load "ledger-conf.el")
(load "mu4e-conf.el")
(load "comint-conf.el")
(load "calendar-conf.el")
(load "taskjuggler-conf.el")

(setq custom-file (concat *domain-custom* "custom.el"))
(load custom-file 'noerror)
(put 'upcase-region 'disabled nil)
