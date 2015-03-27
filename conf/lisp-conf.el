;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Paredit, a mode for editing S-expr based languages  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." 1)

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                clojure-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook (lambda ()
                   (paredit-mode t)
                   (highlight-parentheses-mode t))))

(setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/")

;; bug in emacs 24?
(setq apropos-symbol-face nil)
(setq apropos-label-face nil)
