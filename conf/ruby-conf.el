;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby -- rinari, a mode for rails, ruby and rhtml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *my-default-lib* "/rinari"))
(autoload 'rinari-launch "rinari" nil t)
(autoload 'rinari-minor-mode-map "rinari" nil t)

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
