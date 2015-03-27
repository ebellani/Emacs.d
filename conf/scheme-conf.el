
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gambit
(load-file (concat *my-default-lib* "/gambit.el"))
(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "gsc -:d")

;; adds *.rkt to scheme-mode
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))

;; geiser for racket
;; (add-to-list 'load-path (concat *my-default-lib* "/geiser/elisp/"))
;; (require 'geiser)

;; (eval-after-load "geiser"
;;   '(progn
;;      (require 'geiser-mode)
;;      (setq geiser-racket-use-gracket-p nil)

;;      ;; (define-key geiser-mode-map "\C-c\C-x\C-z" 'geiser-mode-switch-to-repl-and-enter)
;;      ;; add paredit to geiser repl
;;      (add-hook 'geiser-repl-mode-hook (lambda () (paredit-mode +1)))
;;      (setq geiser-racket-extra-keywords
;;            (list "define-syntax-rule"
;;                  "unless"
;;                  "when"
;;                  "with-handlers"
;;                  "class*"
;;                  "super-new"
;;                  "init-field"
;;                  "provide"
;;                  "require"
;;                  "struct"
;;                  "local"
;;                  "define-require-syntax"
;;                  "define/public"
;;                  "define/augment"
;;                  "define/private"
;;                  "define/override"
;;                  "private*"
;;                  "public*"
;;                  "define-values"
;;                  "λ"
;;                  "field"
;;                  "define-runtime-path"))))

;; ;; company, another autocomplete engine. Still testing
;; ;; used primarily as an AC to geiser, an slime like app for scheme.
;; ;; (add-to-list 'load-path (concat *my-default-lib* "/company"))
;; ;; (autoload 'company-mode "company" nil t)

;; (require 'ac-geiser)
;; (add-hook 'geiser-mode-hook 'ac-geiser-setup)
;; (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'geiser-repl-mode))

;; (setq geiser-repl-read-only-prompt-p nil)
;; ;; (setq scheme-program-name "gracket-text")
