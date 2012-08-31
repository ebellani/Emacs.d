;; automatically sets the global mode for all buffers
(global-linum-mode t)

;; flashes the paren close to the cursor.
(show-paren-mode   t)

;; styling. Check if not in terminal to set the nice colors and fonts.
(unless (string= 'nil window-system)
  (progn
    (set-face-font 'default "Inconsolata 12")

    ;; (require 'color-theme)
    ;; (color-theme-initialize)
    ;; (load-file (concat *my-default-lib* "/color-theme-twilight.el"))
    ;; (color-theme-twilight)
    ))



;; hide menus
(menu-bar-mode 0)
(tool-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p 1)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global highlight of whitespace
;; (setq whitespace-style '(face lines-tail))
;; (global-whitespace-mode nil)

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
(global-font-lock-mode t)

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

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)


;; +------------------------------------------------------------+
;; |   highlight-symbol                                         |
;; +------------------------------------------------------------+
;; (highlight-80+-mode 0)
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                clojure-mode-hook
                python-mode
                cc-mode
                haskell-mode))
  (add-hook hook (lambda ()
                   (highlight-symbol-mode))))


;; C style
(add-hook 'c-mode-hook
          '(lambda () (c-set-style "gnu")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-parentheses, a mode for showing where you are  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-file (concat *my-default-lib* "/highlight-parentheses.el"))
;; (require 'highlight-parentheses)

(autoload 'highlight-parentheses-mode "highlight-parentheses" nil)


;; yasnippet, loads of emacs snippets
;; http://code.google.com/p/yasnippet/
;; (yas/initialize)
(yas/load-directory (concat *my-default-lib* "/yasnippet/snippets"))

;; undo-tree
;; treats undo as a tree
(require 'undo-tree)
(global-undo-tree-mode)

;; starts the emacs server so I can access it with emacsclient.
(server-start)


;; help-fns+
;; http://www.emacswiki.org/emacs/help-fns%2b.el
(require 'help-fns+)
