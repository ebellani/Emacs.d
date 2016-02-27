;; automatically sets the global mode for all buffers
(global-linum-mode t)

;; flashes the paren close to the cursor.
(show-paren-mode   t)

;; styling. Check if not in terminal to set the nice colors and fonts.
(unless (string= 'nil window-system)
  (progn
    (set-face-font 'default "Dejavu Sans Mono 10")))

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

;; Keep session
(desktop-save-mode 0)

;; disable the save mode
(global-set-key [f5] 'desktop-save-mode)

;; Inhibit startup window, very annoying
(setq inhibit-startup-message 1)

;; Makes final line always be a return
(setq require-final-newline t)

;; Avoid to make a separate frame
(setq display-buffer nil)
(setq display-buffer-reuse-frames t)
(setq pop-up-frames nil)

;; disable scrollbar
(set-scroll-bar-mode nil)

;; Disable tooltips
(tooltip-mode nil)

;; Make copy and paste to work with other programs
(setq x-select-enable-clipboard t)
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

;; C style
(add-hook 'c-mode-hook
          '(lambda () (c-set-style "gnu")))

;; yasnippet, loads of emacs snippets
(yas-global-mode t)

;; undo-tree
;; treats undo as a tree
(require 'undo-tree)
(global-undo-tree-mode)

;; starts the emacs server so I can access it with emacsclient.
(server-start)

;; help-fns+
;; http://www.emacswiki.org/emacs/help-fns%2b.el
(require 'help-fns+)

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; edit server, a plugin for google chrome
;; http://www.emacswiki.org/emacs/Edit_with_Emacs
(edit-server-start)

;; invoice creation utilities
(autoload 'insert-month-invoices "invoice" "Calls a function to insert entries for invoices.")

;; C source

(setq find-function-C-source-directory
      "/usr/share/emacs/24.4/src")

(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, convert words after that
index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                             (downcase (match-string 0 s)))
                                     t nil s)))
    (downcase s)))

(defun insert-random-uuid ()
  (interactive)
  (shell-command "uuidgen" t))

(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text. See
http://stackoverflow.com/questions/9304192/emacs-linum-mode-and-size-of-font-unreadable-line-numbers"
  (set-window-margins win
                      (ceiling (* (if (boundp 'text-scale-mode-step)
                                      (expt text-scale-mode-step
                                            text-scale-mode-amount) 1)
                                  (if (car (window-margins))
                                      (car (window-margins)) 1)))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)
