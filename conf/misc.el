;; flashes the paren close to the cursor.
(show-paren-mode   t)

;; styling. Check if not in terminal to set the nice colors and fonts.
(unless (string= 'nil window-system)
  (progn
    (set-face-font 'default "Dejavu Sans Mono 12")))

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

;; Inhibit startup window, very annoying
(setq inhibit-startup-message t)

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

;; C source

(setq find-function-C-source-directory
      "/usr/share/emacs/24.4/src")


;; http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing
;; term not autocompleting
(add-hook 'term-mode-hook
          (lambda()
            (yas-minor-mode -1)))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; psession, saving the emacs session so we can use helm frequency stuff
(psession-mode t)
;; don't save kill-ring, that could expose passwords.
(setq psession-object-to-save-alist
      '((ioccur-history . "ioccur-history.el")
        (extended-command-history . "extended-command-history.el")
        (helm-external-command-history . "helm-external-command-history.el")
        (helm-surfraw-engines-history . "helm-surfraw-engines-history.el")
        (psession--save-buffers-alist . "psession-save-buffers-alist.el")
        (helm-ff-history . "helm-ff-history.el")
        (helm-grep-history . "helm-grep-history.el")
        (register-alist . "register-alist.el")
        (psession--winconf-alist . "psession-winconf-alist.el")))

;; set menu as M
(define-key key-translation-map (kbd "<menu>") (kbd "ESC"))
