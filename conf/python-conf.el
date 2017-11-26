;; https://github.com/gporterjamesj/virtualenvwrapper.el

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell)

(add-hook 'inferior-python-mode-hook 'turn-on-comint-history)
