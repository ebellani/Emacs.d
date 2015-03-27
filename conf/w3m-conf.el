;; (setq browse-url-browser-function 'browse-url-generic)
;; (setq browse-url-generic-program "conkeror")
(add-to-list 'load-path (concat *my-default-lib* "/emacs-w3m"))

(setq w3m-use-cookies t)
(setq w3m-key-binding 'info)

(require 'w3m-load)
(require 'w3m-lnum)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
;; (global-set-key "\C-xm" 'browse-url-at-point)

(defun w3m-go-to-linknum ()
  "Turn on link numbers and ask for one to go to. Found on
  http://emacs.wordpress.com/2008/04/12/numbered-links-in-emacs-w3m/"
  (interactive)
  (let ((active w3m-link-numbering-mode))
    (when (not active) (w3m-link-numbering-mode))
    (unwind-protect
        (w3m-move-numbered-anchor (read-number "Anchor number: "))
      (when (not active) (w3m-link-numbering-mode)))))

(add-hook 'w3m-mode-hook 'w3m-lnum-mode)
(define-key w3m-mode-map "f" 'w3m-go-to-linknum)
