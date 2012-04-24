
(auto-save-mode nil)

(setq savehist-file "~/.emacs.d/tmp/savehist")
;; save history in minibuffer
(savehist-mode 1)

;; buffer to html
(autoload 'htmlize-buffer "htmlize" nil t)


;; enable both functions below to enjoy a better autosave.

;; redefining the make-backup-file-name function in order to get
;; backup files in ~/.backups/ rather than scattered around all over
;; the filesystem. Note that you must have a directory ~/.emacs.d/backups/
;; made. This function looks first to see if that folder exists. If
;; it does not the standard backup copy is made.
;; (defun make-backup-file-name (file-name)
;;   "Create the non-numeric backup file name for `file-name'."
;;   (require 'dired)
;;   (let ((backup-location "~/.emacs.d/backups/"))
;;     (if (file-exists-p backup-location)
;; 	(concat (expand-file-name backup-location)
;; 		(replace-regexp-in-string "/" "!" file-name))
;;       (concat file-name "~"))))

;; redefining the make-auto-save-file-name function in order to get
;; autosave files sent to a single directory. Note that this function
;; looks first to determine if you have a ~/.emacs.d/autosaves/ directory. If
;; you do not it proceeds with the standard auto-save procedure.
;; (defun make-auto-save-file-name ()
;;   "Return file name to use for auto-saves of current buffer.."
;;   (if buffer-file-name
;;       (let ((save-location "~/.emacs.d/autosaves/"))
;;         (if (file-exists-p save-location)
;;             (concat (expand-file-name save-location) "#"
;;                     (replace-regexp-in-string "/" "!" buffer-file-name)
;;                     "#")
;;           (concat
;;            (file-name-directory buffer-file-name)
;;            "#"
;;            (file-name-nondirectory buffer-file-name)
;;            "#")))
;;     (expand-file-name
;;      (concat "#%" (buffer-name) "#"))))

;; Preserve the owner and group of the file you're editing
(setq backup-by-copying-when-mismatch t)


;; Remember the position where we closed a file
;; (setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean

;; (setq-default save-place t) ;; activate it for all buffers
;; (require 'saveplace)        ;; get the package
