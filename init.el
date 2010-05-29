(defvar my-default-lib "~/.emacs.d/lib")

(add-to-list 'load-path my-default-lib)

;; vimpulse, because I really like vim's keybindings
(require 'vimpulse)
(require 'redo)

;; automatically sets the global mode for all buffers
(global-linum-mode 1)

;; fonts
(set-face-attribute 'default nil :font "Liberation Mono 10")

;;Setting up tabbar
(require 'tabbar)
(tabbar-mode 1)

;; define C-u and C-d to be like vim (equal pgup pgdown)
;; for some reason C-u was not working
(define-key viper-vi-basic-map "\C-u" 'scroll-down)

;; page-up and page-down scroll bars for the tabs
;; basically modify the keys of tabbar
(global-set-key [C-home] 'tabbar-press-home)

(global-set-key [S-next] 'tabbar-forward-group)
(global-set-key [S-prior] 'tabbar-backward-group)

(global-set-key [C-prior] 'tabbar-backward)
(global-set-key [C-next] 'tabbar-forward)


;; hide menus
(menu-bar-mode -1)
(tool-bar-mode -1)

;; most of the information below was lifted from 
;; http://pintucoperu.wordpress.com/2010/03/04/utilizando-emacs-como-editor-de-texto-para-cc-python-y-vhdl-y-conociendo-el-modo-cua/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep session
;; (desktop-save-mode 1)

;; Inhibit startup window, very annoying
(setq inhibit-startup-message t)

;; Makes final line always be a return
(setq require-final-newline t)

;; Avoid to make a separate frame
(setq display-buffer nil)
(setq display-buffer-reuse-frames t)
(setq pop-up-frames nil)

;; Put scrollbar on the right
(set-scroll-bar-mode 'right)

;; Disable tooltips
;; (tooltip-mode nil)

;; Make copy and paste to work with other programs
(setq x-select-enable-clipboard t)
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
;; (typically in Visual C++)
(global-auto-revert-mode 1)

;; Bell instead of annoying beep
;; (setq visible-bell t)

;; Do not add empty lines at the end of our file if we press down key
(setq next-line-add-newlines nil)

;; When in text (or related mode) break the lines at 80 chars
(setq fill-column 80)

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)

;;   ;; Set indent to 4 instead of 2
;;   (setq standard-indent 4)
;;    
;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;;   ;; Set tab width
;;   (setq default-tab-width 4)

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
;; looks first to determine if you have a ~/.autosaves/ directory. If
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full-screen mode
(defun djcb-full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; autocomplete
;;(add-to-list 'load-path "~/.emacs.d/lib/auto-complete/")
(add-to-list 'load-path (concat my-default-lib "/auto-complete"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat my-default-lib "/auto-complete/ac-dict"))
(ac-config-default)

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)


;; color themes
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)

;;slime configuration
(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path (concat my-default-lib "/slime"))
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; spellchecking flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t) 

(dolist (hook '(lisp-mode-hook 
                elisp-mode-hook 
                ruby-mode-hook
                c-mode-common-hook))
  (add-hook hook (lambda () (flyspell-prog-mode 1))))

(dolist (hook '(text-mode-hook  
                TeX-mode-hook 
                latex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

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


;; http://rinari.rubyforge.org/Basic-Setup.html#Basic-Setup
;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; Rinari
(add-to-list 'load-path (concat my-default-lib "/rinari"))
(require 'rinari)

;;; rhtml-mode
(add-to-list 'load-path (concat my-default-lib "/rhtml"))
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

;; save history in minibuffer 
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/tmp/savehist")
