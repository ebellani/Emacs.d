
;; must have this attribute set or else will complain about missing
;; -l parameter.
;; http://www.emacswiki.org/emacs/InteractiveSpell
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." 1)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." 1)
(autoload 'tex-mode-flyspell-verify "flyspell" "" 1)

;; automatic line breaks and spelling check
(dolist (hook '(text-mode-hook TeX-mode-hook latex-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (auto-fill-mode 1))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(ispell-change-dictionary "american")

(setq flyspell-issue-message-flag t)

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "brasileiro")
                     "american"
                   "brasileiro")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))
