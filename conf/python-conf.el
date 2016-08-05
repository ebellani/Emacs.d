;; (remove-hook 'python-mode-hook
;;   (lambda ()
;;     (setq indent-tabs-mode t)))

(setq python-shell-interpreter "/usr/bin/python3.5")

;; https://github.com/gporterjamesj/virtualenvwrapper.el

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell)
