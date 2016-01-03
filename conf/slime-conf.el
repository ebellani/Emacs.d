;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; switch the name of the eval-after-load file and uncomment
;; the following to switch between clojure and CL.

;; https://jandmworks.com/lisp.html#SBCL quirks
(setq inferior-lisp-program "sbcl")

(defun configure-slime ()
  "Perform all the slime-specific configurations. Here so it can
be called from the clojure-jack-in or just after eval slime from
common lisp."
  (slime-setup '(slime-repl))

  (set-language-environment "UTF-8")
  (setq slime-net-coding-system 'utf-8-unix)
  (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/")
  (global-set-key (kbd "C-c s") 'slime-selector)
  ;; http://emacswiki.org/emacs/ParEdit
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit))


(eval-after-load "slime"
  '(configure-slime))

(dolist (hook '(lisp-mode-hook))
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
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
