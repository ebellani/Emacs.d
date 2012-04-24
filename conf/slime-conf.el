;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://jandmworks.com/lisp.html#SBCL quirks
(add-to-list 'load-path (concat *my-default-lib* "/slime"))
(autoload 'slime-mode "slime" nil)
;; (require slime)
(setq inferior-lisp-program "sbcl")

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl))

     (set-language-environment "UTF-8")
     (setq slime-net-coding-system 'utf-8-unix)
     (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/")
     (global-set-key (kbd "C-c s") 'slime-selector)
     ;; autocomplete with slime's documentation
     (add-to-list 'load-path (concat *my-default-lib* "/ac-slime"))

     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))


(dolist (hook '(lisp-mode-hook
                clojure-mode-hook))
  (add-hook hook (lambda () (slime-mode t))))

(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
   the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n")
    (insert "\n(defpackage :" package "\n  (:use :cl))\n\n")
    (insert "(in-package :" package ")\n\n")))

;; quicklisp slime
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))

;; ;; lush, a numeric lisp
;; (load (concat *my-default-lib* "/lush.el"))
