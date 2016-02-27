(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(autoload 'utop-minor-mode "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-error-after-save nil)

;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

(setq auto-mode-alist (append '(("_oasis" . conf-mode))
                              auto-mode-alist))
