;; http://www.lambdacat.com/modern-emacs-package-management-with-cask-and-pallet/
(require 'cask "~/.cask/cask.el")
(cask-initialize "~/.emacs.d")
(require 'pallet)
(pallet-mode t)
