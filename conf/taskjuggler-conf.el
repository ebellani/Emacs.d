(require 'taskjuggler-mode)

(defun config-tj ()
  (setq tab-width 2))

(add-hook 'taskjuggler-mode-hook #'config-tj)
