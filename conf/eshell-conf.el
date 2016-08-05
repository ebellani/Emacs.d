(setq eshell-directory-name (concat *domain-custom* "/eshell/"))
(setq eshell-history-size 2048)
(add-to-list 'load-path (concat *my-default-lib* "/eshell/"))
(require 'em-bellani)
(require 'em-joc)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier.
from http://www.howardism.org/Technical/Emacs/eshell-fun.html"
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  (delete-window)
  (eshell/exit))

(global-set-key (kbd "C-x !") 'eshell-here)

(add-hook 'eshell-mode-hook (lambda ()
                              (company-mode 0)
                              (linum-mode -1)))
