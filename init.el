(defvar my-default-lib "~/.emacs.d/lib")

(add-to-list 'load-path my-default-lib)

;; vimpulse, because I really like vim's key-bindings
(require 'vimpulse)
(load-file (concat my-default-lib "/viper-boot.el"))
(require 'redo)

;; automatically sets the global mode for all buffers
(global-linum-mode 1)

;; styling. Check if not in terminal to set the nice colors and fonts.
(unless (string= 'nil window-system)
  (progn
    (set-face-attribute 'default nil :font "Liberation Mono 10")
                                        ;      (set-face-attribute 'default nil :font "Anonymous Pro 11")
    (require 'color-theme)
    (color-theme-initialize)
    (load-file (concat my-default-lib "/color-theme-twilight.el"))
    ;; (color-theme-billw)
    (color-theme-twilight)))

;;Setting up tabbar
(require 'tabbar)
(tabbar-mode 1)

;; define C-u and C-d to be like vim (equal pgup pgdown)
;; for some reason C-u was not working
(define-key viper-vi-basic-map "\C-u" 'scroll-down)

;; C-\ adds a lambda symbol, as DrRacket
(define-key viper-insert-global-user-map "\C-\\"
  (lambda () (interactive)
    (insert "λ"))) 

(setq-default viper-electric-mode 1)

;; set the default spacing for ruby editing
(setq viper-shift-width 2)

;; sets C-[ to also mean C-g

;; page-up and page-down scroll bars for the tabs
;; basically modify the keys of tabbar
(global-set-key [C-home] 'tabbar-press-home)

(global-set-key [S-next] 'tabbar-forward-group)
(global-set-key [S-prior] 'tabbar-backward-group)

(global-set-key [C-prior]  'tabbar-backward)
(global-set-key [C-next] 'tabbar-forward)

;; newline also indents
(global-set-key "\r" 'newline-and-indent)

;; hide menus
;; (menu-bar-mode 0)
(tool-bar-mode 0)

;; some of the information below was lifted from
;; http://pintucoperu.wordpress.com/2010/03/04/utilizando-emacs-como-editor-de-texto-para-cc-python-y-vhdl-y-conociendo-el-modo-cua/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p 1)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep session
(desktop-save-mode 1)

;; disable the save mode
(global-set-key [f5] 'desktop-save-mode)

;; Inhibit startup window, very annoying
(setq inhibit-startup-message 1)

;; Makes final line always be a return
;; (setq require-final-newline 1)

;; Avoid to make a separate frame
(setq display-buffer nil)
(setq display-buffer-reuse-frames 1)
(setq pop-up-frames nil)

;; Put scrollbar on the right
(set-scroll-bar-mode 'right)

;; Disable tooltips
;; (tooltip-mode nil)

;; Make copy and paste to work with other programs
(setq x-select-enable-clipboard 1)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; we want fontification in all modes
(global-font-lock-mode t t)

;; maximum possible fontification
(setq font-lock-maximum-decoration t)

;; Provide templates for new files
(auto-insert-mode t)

;; put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- start typing...\n")

;; Automatically reload files after they've been modified
(global-auto-revert-mode 1)

;; When in text (or related mode) break the lines at 80 chars
(setq fill-column 80)

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)

;; Highlight search object
(setq search-highlight           t)

;; Highlight query object
(setq query-replace-highlight    t)

(setq standard-indent 2)

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;; Line by line scrolling
(setq scroll-step 1)

;; Mouse wheel scroll support
(mouse-wheel-mode t)

;; redefining the make-backup-file-name function in order to get
;; backup files in ~/.backups/ rather than scattered around all over
;; the filesystem. Note that you must have a directory ~/.backups/
;; made. This function looks first to see if that folder exists. If
;; it does not the standard backup copy is made.
(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (require 'dired)
  (let ((backup-location "~/.emacs.d/backups/"))
    (if (file-exists-p backup-location)
	(concat (expand-file-name backup-location)
		(replace-regexp-in-string "/" "!" file-name))
      (concat file-name "~"))))

;; redefining the make-auto-save-file-name function in order to get
;; autosave files sent to a single directory. Note that this function
;; looks first to determine if you have a ~/.emacs.d/autosaves/ directory. If
;; you do not it proceeds with the standard auto-save procedure.
(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.."
  (if buffer-file-name
      (let ((save-location "~/.emacs.d/autosaves/"))
        (if (file-exists-p save-location)
            (concat (expand-file-name save-location) "#"
                    (replace-regexp-in-string "/" "!" buffer-file-name)
                    "#")
          (concat
           (file-name-directory buffer-file-name)
           "#"
           (file-name-nondirectory buffer-file-name)
           "#")))
    (expand-file-name
     (concat "#%" (buffer-name) "#"))))

;; Preserve the owner and group of the file you're editing
(setq backup-by-copying-when-mismatch t)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Remember the position where we closed a file
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean

(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace) ;; get the package

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)

;; Full-screen mode
(defun djcb-full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; set key for the function above
(global-set-key (kbd "<f11>") 'djcb-full-screen-toggle)

;; autocomplete
(add-to-list 'load-path (concat my-default-lib "/auto-complete"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat my-default-lib "/auto-complete/ac-dict"))
(ac-config-default)


;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))

;; tab in insert mode calls autocomplete
(ac-set-trigger-key "TAB")
;; (define-key viper-insert-global-user-map (kbd "<tab>") 'auto-complete)

(real-global-auto-complete-mode 1)

;; quack configuration
;; (require 'quack)

;;slime configuration
(add-to-list 'load-path (concat my-default-lib "/slime"))
(require 'slime)
(slime-setup '(slime-repl))
(setq inferior-lisp-program "sbcl")
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(global-set-key (kbd "C-c s") 'slime-selector)

;; To make SLIME connect to your lisp whenever you open a lisp file just add
;; this to your .emacs:
;; http://common-lisp.net/project/slime/doc/html/Auto_002dSLIME.html#Auto_002dSLIME
;; (add-hook 'slime-mode-hook
;;           (lambda ()
;;             (unless (slime-connected-p)
;;               (save-excursion (slime)))))

;; spell-checking flyspell

;; must have this attribute set or else will complain about missing
;; -l parameter.
;; http://www.emacswiki.org/emacs/InteractiveSpell
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." 1)

(autoload 'flyspell-delay-command "flyspell" "Delay on command." 1)
(autoload 'tex-mode-flyspell-verify "flyspell" "" 1)

;;(dolist (hook '(lisp-mode-hook
;;                elisp-mode-hook
;;                ruby-mode-hook
;;                c-mode-common-hook))
;;  (add-hook hook (lambda () (flyspell-prog-mode 1))))

;; automatic line breaks and spelling check
(dolist (hook '(text-mode-hook TeX-mode-hook latex-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (auto-fill-mode 1))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(ispell-change-dictionary "american")

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "brasileiro")
                     "american"
                   "brasileiro")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>") 'fd-switch-dictionary)


;; C style
(add-hook 'c-mode-hook
          '(lambda () (c-set-style "gnu")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Maps swaps [ for ( and vice versa                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Paredit, a mode for editing S-expr based languages  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." 1)

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                ruby-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook (lambda () (paredit-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scheme-program-name "mzscheme")


;; http://iinari.rubyforge.org/Basic-Setup.html#Basic-Setup
(require 'ido)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rinari, a mode for rails, ruby and rhtml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat my-default-lib "/rinari"))
(require 'rinari)

(defun my-insert-erb-skeleton ()
  (interactive)
  (rinari-insert-erb-skeleton 1))

(define-key rinari-minor-mode-map "\C-c;a" 'my-insert-erb-skeleton)

;; add newline and indent to enter
(define-key rinari-minor-mode-map "\r" 'newline-and-indent)

;;; rhtml-mode
(add-to-list 'load-path (concat my-default-lib "/rhtml"))
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda ()
            (rinari-launch)))

(setq savehist-file "~/.emacs.d/tmp/savehist")
;; save history in minibuffer
(savehist-mode 1)

;; buffer to html
(require 'htmlize)

;; javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'js-comint)
(setq inferior-js-program-command "node")
(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)))



;; yasnippet, loads of emacs snippets
;; http://code.google.com/p/yasnippet/
(add-to-list 'load-path (concat my-default-lib "/yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat my-default-lib "/yasnippet/snippets"))

;; undo-tree
;; treats undo as a tree
(require 'undo-tree)
(global-undo-tree-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ruby-indent-level 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
