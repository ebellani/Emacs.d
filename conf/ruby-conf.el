;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby -- rinari, a mode for rails, ruby and rhtml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "rinari-launch"
  '(progn
     (defun my-insert-erb-skeleton ()
       (interactive)
       (rinari-insert-erb-skeleton 1))

     (define-key rinari-minor-mode-map "\C-c;a" 'my-insert-erb-skeleton)
     ;; add newline and indent to enter
     (define-key rinari-minor-mode-map "\r" 'newline-and-indent)
     (add-to-list 'load-path (concat *my-default-lib* "/rhtml"))
     (require 'rhtml-mode)))

(setq ruby-indent-level 2)

(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;; (load "ruby-compilation-rspec.elc")

;;(require  'ruby-compilation-rspec)
