(defvar *my-default-lib* "~/.emacs.d/lib")

(add-to-list 'load-path *my-default-lib*)

(require 'redo)
(global-set-key [C-?] 'undo)

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
    ;; (load-file (concat *my-default-lib* "/color-theme-twilight.el"))
    (load-file (concat *my-default-lib* "/zenburn-el/zenburn.el"))
    ;; ;; theme for darker enviroments

    ;; (color-theme-twilight)
    (color-theme-zenburn)
    ;; theme for lighted enviroments
    ;; (color-theme-greiner)
    ;; greiner is cool in a lighted enviroment.
    ))


;; C-\ adds a lambda symbol, as DrRacket
;; (define-key global-map "\C-\\"
;;   (lambda () (interactive)
;;           (insert "λ")))

;; newline also indents
(global-set-key "\r" 'newline-and-indent)


;; hide menus
(menu-bar-mode 0)
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
  (let ((backup-location "~/.backups/"))
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
      (let ((save-location "~/.autosaves/"))
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
(setq save-place-file "~/.saveplace") ;; keep my ~/ clean

(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace) ;; get the package

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)

;; comment and uncomment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; +-------------------------------------------------------+
;; |                                                       |
;; |                    eshell                             |
;; |                                                       |
;; +-------------------------------------------------------+

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

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
;; slime configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://jandmworks.com/lisp.html#SBCL quirks
(add-to-list 'load-path (concat *my-default-lib* "/slime"))
(require 'slime)
(slime-setup '(slime-repl))
(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(global-set-key (kbd "C-c s") 'slime-selector)

(setq common-lisp-hyperspec-root
      "file:/usr/share/doc/hyperspec/")

;; autocomplete with slime's documentation
(add-to-list 'load-path (concat *my-default-lib* "/ac-slime"))

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)


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

;; To make SLIME connect to your lisp whenever you open a lisp file just add
;; this to your .emacs:
;; http://common-lisp.net/project/slime/doc/html/Auto_002dSLIME.html#Auto_002dSLIME
;; (add-hook 'slime-mode-hook
;;           (lambda ()
;;             (unless (slime-connected-p)
;;               (save-excursion (slime)))))


;; quicklisp slime
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))

;; lush, a numeric lisp
;; (load (concat *my-default-lib* "/lush.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things taken from http://sites.google.com/site/steveyegge2/effective-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; item #2
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; item #3
(global-set-key "\C-w" 'backward-kill-word)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-parentheses, a mode for showing where you are  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (concat *my-default-lib* "/highlight-parentheses.el"))
(require 'highlight-parentheses)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Paredit, a mode for editing S-expr based languages  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." 1)
(require 'paredit)

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook (lambda () (paredit-mode 1))))



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
(load-file (concat *my-default-lib* "/geiser/elisp/geiser.el"))
(require 'geiser-mode)

;; (setq scheme-program-name "gracket-text")

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
            "define-runtime-path"))

;; pretty things up
(defconst schemecmpct-lambda-char    (decode-char 'ucs #x03BB))
;; (make-char 'greek-iso8859-7 107)
(defconst schemecmpct-delta-char     (decode-char 'ucs #x03B4))
;; (make-char 'greek-iso8859-7 100)

(defconst schemecmpct-upper-sigma-char (decode-char 'ucs #x03a3))

(defconst schemecmpct-beta-char      (decode-char 'ucs #x03B2))
;; 'greek-iso8859-7 98))
(defconst schemecmpct-xor-char       (decode-char 'ucs #x22BB))
(defconst schemecmpct-rho-char       (decode-char 'ucs #x03C1))
(defconst schemecmpct-sigma-char     (decode-char 'ucs #x03C2))
(defconst schemecmpct-iota-char      (decode-char 'ucs #x03B9))
(defconst schemecmpct-log-and-char   (decode-char 'ucs #x2227))
(defconst schemecmpct-log-or-char    (decode-char 'ucs #x2228))
(defconst schemecmpct-log-not-char   (decode-char 'ucs #x00AC))
(defconst schemecmpct-log-true-char  (decode-char 'ucs #x22A4))
(defconst schemecmpct-log-false-char (decode-char 'ucs #x22A5))
(defconst schemecmpct-less-equal-char    (decode-char 'ucs #x2264))
(defconst schemecmpct-greater-equal-char (decode-char 'ucs #x2265))

(defconst schemecmpct-key-words
  '(("[[(]\\(case-\\|match-\\|opt-\\)?\\(lambda\\)\\>"
     2
     (progn (compose-region (match-beginning 2)
                            (match-end       2)
                            schemecmpct-lambda-char)
            nil))
    ("[[(]\\(define\\)\\(-.+\\)?\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-delta-char)
            nil))
    ("[[(]\\(let\\)\\(*\\|-\\)?"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-beta-char)
            nil))
    ("[[(]\\(set!\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-sigma-char)
            nil))
    ("[[(]\\(begin\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-rho-char)

            nil))
    ("[[(]\\(if\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-iota-char)
            nil))
    ("[[(]\\(for \\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-upper-sigma-char)
            nil))
    ("[[(]\\(for/\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (1- (match-end       1))
                            schemecmpct-upper-sigma-char)
            nil)) 
    ("\\(-and\\)"
     1
     (progn (compose-region (1+ (match-beginning 1))
                            (match-end       1)
                            schemecmpct-log-and-char)
            nil))
    ("[[(]\\(and\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-log-and-char)
            nil))
    ("\\(-xor\\)"
     1
     (progn (compose-region (1+ (match-beginning 1))
                            (match-end       1)
                            schemecmpct-xor-char)
            nil))
    ("\\(-or\\)"
     1
     (progn (compose-region (1+ (match-beginning 1))
                            (match-end       1)
                            schemecmpct-log-or-char)
            nil))
    ("[[(]\\(or\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-log-or-char)
            nil))
    ("[[(]\\(-not\\)\\>"
     1
     (progn (compose-region (1+ (match-beginning 1))
                            (match-end       1)
                            schemecmpct-log-not-char)
            nil))
    ("[[(]\\(not\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-log-not-char)
            nil))
    ("[[(]\\(<=\\)"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-less-equal-char)
            nil))
    ("[[(]\\(>=\\)"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-greater-equal-char)
            nil))
    ("\\(#t\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                            (match-end       1)
                            schemecmpct-log-true-char)
            nil))
    ("\\(#f\\)\\>"
     1
     (progn (compose-region (match-beginning 1)
                             (match-end       1)
                            schemecmpct-log-false-char)
            nil))))

(defun schemecmpct-install-fontification ()
  "Install font lock extensions to scheme mode."
  (font-lock-add-keywords nil schemecmpct-key-words))

;; (add-hook 'scheme-mode-hook 'schemecmpct-install-fontification)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode 1)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote
                       ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby -- rinari, a mode for rails, ruby and rhtml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *my-default-lib* "/rinari"))
(require 'rinari)

(defun my-insert-erb-skeleton ()
  (interactive)
  (rinari-insert-erb-skeleton 1))

(define-key rinari-minor-mode-map "\C-c;a" 'my-insert-erb-skeleton)

;; add newline and indent to enter
(define-key rinari-minor-mode-map "\r" 'newline-and-indent)

;;; rhtml-mode
(add-to-list 'load-path (concat *my-default-lib* "/rhtml"))
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda ()
            (rinari-launch)))

(setq ruby-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq savehist-file "~/.emacs.d/tmp/savehist")
;; save history in minibuffer
(savehist-mode 1)

;; buffer to html
(require 'htmlize)



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

;; (autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; (require 'js-comint)
;; (setq inferior-js-program-command "node")
;; (add-hook 'js2-mode-hook '(lambda ()
;; 			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;; 			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;; 			    (local-set-key "\C-cb" 'js-send-buffer)
;; 			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;; 			    (local-set-key "\C-cl" 'js-load-file-and-go)))



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
(setq browse-url-generic-program "conkeror")

;; +----------------------------------------------------------------+
;; |              i-menu-mode, with ido support                     |
;; | http://www.emacswiki.org/emacs/ImenuMode#toc10                 |
;; +----------------------------------------------------------------+
(load-file (concat *my-default-lib* "/idomenu.el"))

;; http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/
(global-set-key (kbd "M-i") 'idomenu)

;; +-------------------------------------------------------------------------+
;; |                        VisibleMark                                      |
;; |           http://www.emacswiki.org/emacs/VisibleMark                    |
;; +-------------------------------------------------------------------------+
;; (load-file (concat *my-default-lib* "/visible-mark.el"))
;; (require 'visible-mark)
;; (setq-default visible-mark-mode t)


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


;; +----------------------------------------------------------------+
;; |          legalese, insert legal licenses.                      |
;; |      http://www.emacswiki.org/emacs/legalese.el                |
;; +----------------------------------------------------------------+
(load-file (concat *my-default-lib* "/legalese.el"))
;; (require 'legalese)


(put 'downcase-region 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(legalese-default-license (quote fdl))
 '(safe-local-variable-values (quote ((erlang-indent-level . 4) (c-file-style . k&r) (c-font-lock-extra-types "FILE" "\\sw+_t" "at" "gptr" "real" "flt" "intg") (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby")))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
