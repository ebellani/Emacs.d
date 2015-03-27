

;; +---------------------------------------------------------------------------+
;; |                                                                           |
;; |                                                                           |
;; |                          javascript & mozrepl                             |
;; |                                                                           |
;; |                                                                           |
;; +---------------------------------------------------------------------------+
;; (autoload 'js2-mode "js2" nil t)

;;     C-c C-s: open a MozRepl interaction buffer and switch to it
;;     C-c C-l: save the current buffer and load it in MozRepl
;;     C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;;     C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;;     C-c C-r: send the current region to MozRepl

;; In the interaction buffer:

;;     C-c c: insert the current name of the REPL plus the dot operator (usually repl.)

;; (autoload 'moz-minor-mode "moz"
;;   "Mozilla Minor and Inferior Mozilla Modes" t)

;; (add-hook 'js2-mode-hook 'js2-custom-setup)
;; (defun js2-custom-setup ()
;;   (moz-minor-mode 1))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js-mode-hook (lambda () (smartparens-mode t)))
