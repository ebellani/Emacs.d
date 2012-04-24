;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs

;; common lisp compatibility
(require 'cl) 

(defvar *emacs-load-start* (current-time))

(defvar *my-default-lib* "~/.emacs.d/lib"
  "Vendor libraries that cannot be installed via the package system")
(defvar *my-conf* "~/.emacs.d/conf/"
  "My configurations")

(add-to-list 'load-path *my-default-lib*)
(add-to-list 'load-path *my-conf*)


(load "package-manager.el")
(load "misc.el")
(load "history.el")
(load "keybindings.el")
(load "ido-conf.el")
(load "autocompletion.el")
(load "eshell-conf.el")
(load "slime-conf.el")
(load "spell-checking.el")
(load "lisp-conf.el")
(load "scheme-conf.el")
(load "ruby-conf.el")
(load "js-conf.el")
(load "haskell-conf.el")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; yasnippet, loads of emacs snippets
;; http://code.google.com/p/yasnippet/
;; (yas/initialize)
(yas/load-directory (concat *my-default-lib* "/yasnippet/snippets"))

;; undo-tree
;; treats undo as a tree
(require 'undo-tree)
(global-undo-tree-mode)

;; starts the emacs server so I can access it with emacsclient.
(server-start)

;; default browser is conkeror, so we can get a truly smooth emacs
;; cult going.
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")




;; +------------------------------------------------------------+
;; |        magit, a git mode                                   |
;; +------------------------------------------------------------+
;; (require 'magit)

;; ;; +------------------------------------------------------------+
;; ;; |                       Erlang                               |
;; ;; +------------------------------------------------------------+
;; (add-to-list 'load-path (concat *my-default-lib* "/erlang"))
;; (require 'erlang-start)
;; (add-to-list 'load-path  (concat *my-default-lib* "/distel/elisp"))

;; (require 'distel)
;; (distel-setup)


;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file 'noerror)


;; find out the time your emacs took to load.
;; (message "My .emacs loaded in %ds"
;;          (destructuring-bind (hi lo ms) (current-time)
;;            (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
