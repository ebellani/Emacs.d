
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adds *.rkt to scheme-mode
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))

;; geiser for racket
;; (add-to-list 'load-path (concat *my-default-lib* "/geiser/elisp/"))
;; (require 'geiser)

(eval-after-load "geiser"
  '(progn
     (require 'geiser-mode)
     (setq geiser-racket-use-gracket-p nil)
     ;; add paredit to geiser repl
     (add-hook 'geiser-repl-mode-hook (lambda () (paredit-mode +1)))
     (setq geiser-racket-extra-keywords
           (list "define-syntax-rule"
                 "match*"
                 "unless"
                 "when"
                 "with-handlers"
                 "class*"
                 "super-new"
                 "init-field"
                 "provide"
                 "require"
                 "struct"
                 "local"
                 "define-require-syntax"
                 "define/public"
                 "define/augment"
                 "define/private"
                 "define/override"
                 "private*"
                 "public*"
                 "define-values"
                 "λ"
                 "field"
                 "define-type"
                 "define-runtime-path"))))

(defun force-refresh-buffer ()
  (end-of-buffer)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

(defun with-refreshed-buffer (fun)
  (save-excursion
    (force-refresh-buffer))
  (funcall fun))

(defun geiser-refresh-and-mode-switch-to-repl-and-enter ()
  (interactive)
  (with-refreshed-buffer #'geiser-mode-switch-to-repl-and-enter))

(defun geiser-refresh-compile-current-buffer ()
  (interactive)
  (with-refreshed-buffer #'geiser-compile-current-buffer))

(defun customize-geiser-map ()
  (define-key geiser-mode-map (kbd "C-c C-a") 'geiser-refresh-and-mode-switch-to-repl-and-enter)
  (define-key geiser-mode-map (kbd "C-c C-k") 'geiser-refresh-compile-current-buffer))

(require 'scribble)
(setq auto-mode-alist (cons '("\\.scrbl$" . scribble-mode) auto-mode-alist))
