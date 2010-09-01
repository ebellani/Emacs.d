;; Hack to get *Messages* in viper-mode.
;; (must be done after loading viper)
;; Futzing with fundamental-mode doesn't seem to help.
(save-excursion
  (set-buffer "*Messages*")
  (viper-change-state-to-vi))

;; Make Emacs mode stick out more in status bar.
(setq viper-emacs-state-id
      (concat (propertize "<EMACS>" 'face 'isearch) " "))

; Show when minibuffer is in Emacs mode.
(when (facep 'viper-minibuffer-emacs)
  (set-face-foreground 'viper-minibuffer-emacs "white")
  (set-face-background 'viper-minibuffer-emacs "black"))

;; Viper is overreaching by caring whether a visited file is under version
;; control -- disable this check.
(defadvice viper-maybe-checkout (around viper-vcs-check-is-retarded activate)
  nil)

;; Remove read-only property from pasted text -- in newer versions of slime,
;; output to slime-repl is read-only and I often want to modify text I copied
;; from there.
(dolist (fn '(viper-put-back viper-Put-back))
  (eval `(defadvice ,fn (around steve-remove-read-only activate)
           (let ((text
                  ;; Taken from viper-put-back
                  (if viper-use-register
                      (cond ((viper-valid-register viper-use-register '(digit))
                             (current-kill
                              (- viper-use-register ?1) 'do-not-rotate))
                            ((viper-valid-register viper-use-register)
                             (get-register (downcase viper-use-register)))
                            (t (error viper-InvalidRegister 
viper-use-register)))
                      (current-kill 0))))
             (when text
               (put-text-property 0 (length text) 'read-only nil text)))
           ad-do-it)))

;; Map ":" to M-x.  Drop viper command mode.
(define-key viper-vi-global-user-map ":" 'execute-extended-command)
(define-key viper-vi-global-user-map ";" 'execute-extended-command)

;; Add more viper-ified modes
(setq viper-vi-state-mode-list
      (append viper-vi-state-mode-list
              '(grep-mode
                comint-mode
                slime-xref-mode
                slime-repl-mode
                completion-list-mode)))

;; SLIME macroexpansion mode -- force change to viper
(defun steve-slime-temp-buffer-fixes ()
  (when (string-equal (buffer-name) "*SLIME Macroexpansion*")
    (viper-change-state-to-vi)))
(add-hook 'lisp-mode-hook 'steve-slime-temp-buffer-fixes)
(add-hook 'clojure-mode-hook 'steve-slime-temp-buffer-fixes)

;; SLIME REPL fixes
(add-hook 'slime-repl-mode-hook 'viper-comint-mode-hook)

;; SLIME *slime-events* fix
(add-hook 'slime-connected-hook
          (lambda ()
            (save-excursion
              (set-buffer (slime-events-buffer))
              (viper-change-state-to-vi))))
