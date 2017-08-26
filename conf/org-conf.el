;; a lot of this came from http://doc.norang.ca/org-mode.html

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-refile-allow-creating-parent-nodes 'confirm)


(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; set the default org directory

(setq org-directory (concat *domain-custom* "org/"))
(setq refile-file-path (concat org-directory "refile.org"))

(setq org-agenda-files
      `(,refile-file-path
        ,(concat org-directory "personal.org")))

(setq org-tag-alist '((:startgroup)
                      ("noexport" . ?n)
                      ("export" . ?e)
                      (:endgroup)
                      ("BILLABLE")))

(setq org-capture-templates
      `(("t" "todo" entry (file ,refile-file-path)
         "* TODO %?" :empty-lines 1)
        ("r" "review" entry (file ,refile-file-path)
         "* %?\n")
        ("s" "schedule" entry (file ,schedule-file-path)
         "* %?\n\n %^T" :kill-buffer t)))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 9)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

;;; navigation

(setq org-imenu-depth 6)

;;; bibtex

;; using bibtex
;; http://orgmode.org/worg/org-faq.html#using-reftex-in-org-mode
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;;; babel

(setq org-src-fontify-natively t)

(add-to-list 'org-src-lang-modes (quote ("racket" . scheme)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot     . t)
   (shell   . t)
   (python  . t)
   (js      . t)
   (ocaml   . t)
   (java    . t)
   (scheme  . t)
   (plantuml . t)
   (ditaa   . t)
   (sqlite  . t)
   (gnuplot . t)
   (ditaa  . t)
   (C      . t)
   (ledger . t)
   (org    . t)))

;;; disable confirmation of evaluation of code. CAREFUL WHEN EVALUATING
;;; FOREIGN ORG FILES!

(setq org-confirm-babel-evaluate nil)

;;; Listings for src blocks

(setq org-latex-listings t)
(setq org-latex-packages-alist '(("" "listings")
                                 ("" "color")))

(setq org-latex-listings-options
      '(("frame" "single")
        ("basicstyle" "\\footnotesize")
        ("numbers" "left")
        ("numberstyle" "\\tiny")
        ("breaklines" "true")))

;;; subscripts

(setq org-use-sub-superscripts '{})
(setq org-export-with-sub-superscripts '{})

;; initial buffer is agenda
(add-hook 'after-init-hook (lambda ()
                             (org-agenda t "n")
                             (delete-other-windows)))

(setq org-babel-default-header-args
      (cons '(:noweb . "yes")
            (assq-delete-all :noweb org-babel-default-header-args)))

(setq org-babel-default-header-args
      (cons '(:tangle . "yes")
            (assq-delete-all :tangle org-babel-default-header-args)))

(setq org-babel-default-header-args
      (cons '(:comments . "link")
            (assq-delete-all :comments org-babel-default-header-args)))

;; emacs lisp

(defun disable-fylcheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-src-mode-hook 'disable-fylcheck-in-org-src-block)

(defun org-table-time-seconds-to-string (secs &optional output-format)
  "Mofifies org function to truncate the seconds"
  (let* ((secs0 (abs secs))
         (res
          (cond ((eq output-format 'days)
                 (format "%.3f" (/ (float secs0) 86400)))
                ((eq output-format 'hours)
                 (format "%.2f" (/ (float secs0) 3600)))
                ((eq output-format 'minutes)
                 (format "%.1f" (/ (float secs0) 60)))
                ((eq output-format 'seconds)
                 (format "%d" secs0))
                (t (org-format-seconds "%.2h:%.2m" secs0)))))
    (if (< secs 0) (concat "-" res) res)))

;; better timestamp for export
;; http://endlessparentheses.com/better-time-stamps-in-org-export.html
(add-to-list 'org-export-filter-timestamp-functions
             #'endless/filter-timestamp)
(setq-default org-display-custom-times t)
;;; Before you ask: No, removing the <> here doesn't work.
(setq org-time-stamp-custom-formats
      '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
(defun endless/filter-timestamp (trans back _comm)
  "Remove <> around time-stamps."
  (pcase back
    ((or `jekyll `html)
     (replace-regexp-in-string "&[lg]t;" "" trans))
    ((or `latex `ascii)
     (replace-regexp-in-string "[<>]" "" trans))))
