(add-to-list 'auto-mode-alist '("\.dat$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\.ledger$" . ledger-mode))

(eval-after-load 'flycheck '(require 'flycheck-ledger))
