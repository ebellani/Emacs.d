;; +------------------------------------------------------------+
;; |                       Haskell                              |
;; +------------------------------------------------------------+
(eval-after-load "haskell-mode"
  '(progn
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
     (setq haskell-program-name "ghci")))
