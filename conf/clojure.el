;; compojure indentation.
;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(add-hook 'cider-mode-hook 'paredit-mode)

;; clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.emacs.d/cider-repl-history")
