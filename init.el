;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs

;; common lisp compatibility
(require 'cl) 

(defvar *emacs-load-start* (current-time))


(defvar *my-default-lib* "~/.emacs.d/lib")
(add-to-list 'load-path *my-default-lib*)

;; automatic elisp package management & archive
;; http://marmalade-repo.org/

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)



;; automatically sets the global mode for all buffers
(global-linum-mode t)

;; flashes the paren close to the cursor.
(show-paren-mode   t)

;; styling. Check if not in terminal to set the nice colors and fonts.
(unless (string= 'nil window-system)
  (progn
    (set-face-font 'default "Inconsolata 12")
    (require 'color-theme)
    (color-theme-initialize)
    (load-file (concat *my-default-lib* "/color-theme-twilight.el"))
    (load-file (concat *my-default-lib* "/zenburn-el/zenburn.el"))
    ;; ;; theme for darker enviroments

    ;; (color-theme-twilight)
    (color-theme-zenburn)
    ;; theme for lighted enviroments
    ;; (color-theme-greiner)
    ;; greiner is cool in a lighted enviroment.w9b
    ))


;; C-\ adds a lambda symbol, as DrRacket
(define-key global-map "\C-\\"
  (lambda () (interactive)
          (insert "λ")))

;; newline also indents
(global-set-key "\r" 'newline-and-indent)


;; hide menus
(menu-bar-mode 1)
(tool-bar-mode 0)

;; some of the information below was lifted from
;; http://pintucoperu.wordpress.com/2010/03/04/utilizando-emacs-como-editor-de-texto-para-cc-python-y-vhdl-y-conociendo-el-modo-cua/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(winner-mode 1)
(global-set-key (kbd "C-x <left>")  'windmove-left) ; move to left windnow
(global-set-key (kbd "C-x <right>") 'windmove-right) ; move to right window
(global-set-key (kbd "C-x <up>")    'windmove-up)     ; move to upper window
(global-set-key (kbd "C-x <down>")  'windmove-down)    ; move to downer window


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
;; (global-font-lock-mode t t)

;; ;; maximum possible fontification
;; (setq font-lock-maximum-decoration t)

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

;; comment and uncomment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; +-------------------------------------------------------+
;; |                                                       |
;; |                   icicles                             |
;; |                                                       |
;; +-------------------------------------------------------+
(add-to-list 'load-path (concat *my-default-lib* "/icicles"))
(require 'icicles)
(icy-mode t)


;; install script
;; (load (concat *my-default-lib* "/icicles-install.el"))

;; +-------------------------------------------------------+
;; |                                                       |
;; |                    eshell                             |
;; |                                                       |
;; +-------------------------------------------------------+

(add-to-list 'load-path (concat *my-default-lib* "/eshell"))
(require 'em-bellani)
(require 'em-joc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *my-default-lib* "/auto-complete"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat *my-default-lib* "/auto-complete/ac-dict"))
(ac-config-default)

;; dirty fix for having AC everywhere
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                          (auto-complete-mode 1))
;;                        ))

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (or (minibufferp (current-buffer))
                                    (not (numberp
                                          (compare-strings "*eshell*" 0 7
                                                           (buffer-name
                                                            (current-buffer)) 0 7)))))
                         (auto-complete-mode 1))))

;; tab in insert mode calls autocomplete
(ac-set-trigger-key "TAB")

(real-global-auto-complete-mode 1)

;; company, another autocomplete engine. Still testing
;; used primarily as an AC to geiser, an slime like app for scheme.
(add-to-list 'load-path (concat *my-default-lib* "/company"))
(autoload 'company-mode "company" nil t)

(add-hook 'scheme-mode-hook
          (lambda () (company-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clojure-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://jandmworks.com/lisp.html#SBCL quirks
(add-to-list 'load-path (concat *my-default-lib* "/slime"))
(autoload 'slime-mode "slime" nil)

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl))
     (setq inferior-lisp-program "sbcl")
     (set-language-environment "UTF-8")
     (setq slime-net-coding-system 'utf-8-unix)
     (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/")
     (global-set-key (kbd "C-c s") 'slime-selector)
     ;; autocomplete with slime's documentation
     (add-to-list 'load-path (concat *my-default-lib* "/ac-slime"))

     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))


(dolist (hook '(lisp-mode-hook
                clojure-mode-hook))
  (add-hook hook (lambda () (slime-mode t))))

(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
   the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n")
    (insert "\n(defpackage :" package "\n  (:use :cl))\n\n")
    (insert "(in-package :" package ")\n\n")))

;; quicklisp slime
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))

;; ;; lush, a numeric lisp
;; (load (concat *my-default-lib* "/lush.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things taken from http://sites.google.com/site/steveyegge2/effective-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; item #2
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm"    'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; item #3
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spell-checking flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-parentheses, a mode for showing where you are  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-file (concat *my-default-lib* "/highlight-parentheses.el"))
;; (require 'highlight-parentheses)

(autoload 'highlight-parentheses-mode "highlight-parentheses" nil)

;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   (lambda ()
;;     (highlight-parentheses-mode t)))
;; (global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Paredit, a mode for editing S-expr based languages  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." 1)

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                clojure-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook (lambda ()
                   (paredit-mode t)
                   (highlight-parentheses-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gambit
;; (load-file (concat *my-default-lib* "/gambit.el"))
;; (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;; (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;; (add-hook 'scheme-mode-hook (function gambit-mode))
;; (setq scheme-program-name "gsc -:d")

;; geiser for racket
(autoload 'geiser (concat *my-default-lib* "/geiser/elisp/geiser.el") nil t)

(eval-after-load "geiser"
  '(progn
     (require 'geiser-mode)
     (setq geiser-racket-use-gracket-p nil)

     ;; adds *.rkt to scheme-mode
     (setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))

     ;; (define-key geiser-mode-map "\C-c\C-x\C-z" 'geiser-mode-switch-to-repl-and-enter)

     (setq geiser-racket-extra-keywords
           (list "define-syntax-rule"
                 "unless"
                 "when"
                 "with-handlers"
                 "class*"
                 "super-new"
                 "init-field"
                 "provide"
                 "require"
                 "struct"
                 "local"
                 "define-require-syntax"
                 "define/public"
                 "define/augment"
                 "define/override"
                 "define-values"
                 "λ"
                 "field"
                 "define-runtime-path"))))

;; (setq scheme-program-name "gracket-text")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby -- rinari, a mode for rails, ruby and rhtml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *my-default-lib* "/rinari"))
(autoload 'rinari-launch "rinari" nil t)
(autoload 'rinari-minor-mode-map "rinari" nil t)

(eval-after-load "rinari-launch"
  '(progn
     (defun my-insert-erb-skeleton ()
       (interactive)
       (rinari-insert-erb-skeleton 1))
     
     (define-key rinari-minor-mode-map "\C-c;a" 'my-insert-erb-skeleton)
     ;; add newline and indent to enter
     (define-key rinari-minor-mode-map "\r" 'newline-and-indent)
     (add-to-list 'load-path (concat *my-default-lib* "/rhtml"))
     (require 'rhtml-mode)))

(setq ruby-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq savehist-file "~/.emacs.d/tmp/savehist")
;; save history in minibuffer
(savehist-mode 1)

;; buffer to html
(autoload 'htmlize-buffer "htmlize" nil t)


;; +---------------------------------------------------------------------------+
;; |                                                                           |
;; |                                                                           |
;; |                          javascript & mozrepl                             |
;; |                                                                           |
;; |                                                                           |
;; +---------------------------------------------------------------------------+
(autoload 'js2-mode "js2" nil t)

;;     C-c C-s: open a MozRepl interaction buffer and switch to it
;;     C-c C-l: save the current buffer and load it in MozRepl
;;     C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;;     C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;;     C-c C-r: send the current region to MozRepl

;; In the interaction buffer:

;;     C-c c: insert the current name of the REPL plus the dot operator (usually repl.)

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'js2-mode-hook 'js2-custom-setup)
(defun js2-custom-setup ()
  (moz-minor-mode 1))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; yasnippet, loads of emacs snippets
;; http://code.google.com/p/yasnippet/
(add-to-list 'load-path (concat *my-default-lib* "/yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat *my-default-lib* "/yasnippet/snippets"))

;; undo-tree
;; treats undo as a tree
(require 'undo-tree)
(global-undo-tree-mode)

;; starts the emacs server so I can access it with emacsclient.
(server-start)

;; default browser is conkeror, so we can get a truly smooth emacs
;; cult going.
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")

;; +----------------------------------------------------------------+
;; |              i-menu-mode, with ido support                     |
;; | http://www.emacswiki.org/emacs/ImenuMode#toc10                 |
;; +----------------------------------------------------------------+
(autoload 'idomenu "idomenu.el" t)
;; http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/
(global-set-key (kbd "M-i") 'idomenu)

;; +------------------------------------------------------------+
;; |        cua-mode                                            |
;; +------------------------------------------------------------+
;; (cua-mode t)

;; +------------------------------------------------------------+
;; |                       Erlang                               |
;; +------------------------------------------------------------+
(add-to-list 'load-path (concat *my-default-lib* "/erlang"))
(require 'erlang-start)
(add-to-list 'load-path  (concat *my-default-lib* "/distel/elisp"))

(require 'distel)
(distel-setup)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; find out the time your emacs took to load.
;; (message "My .emacs loaded in %ds"
;;          (destructuring-bind (hi lo ms) (current-time)
;;            (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
