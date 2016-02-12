;; a lot of this came from http://doc.norang.ca/org-mode.html

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-refile-allow-creating-parent-nodes 'confirm)


;; Use full outline paths for refile targets - we file directly with
;; IDO
(setq org-refile-use-outline-path 'file)

(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido t)

(setq org-directory "~/org/")

(setq org-agenda-files
      '("~/org/refile.org"
        ;; "~/org/personal/"
        ;; work vs personal, uncomment/comment
        "~/Projects/brickabode/org-issues/201602.org"
        ))

(setq org-tag-alist '((:startgroup)
                      ("@neoway" .    ?n)
                      ("@home" .      ?h)
                      (:endgroup)
                      (:startgroup)
                      ("WRITING" .    ?w)
                      ("CODE" .       ?c)
                      ("SOCIAL" .     ?s)
                      ("RESEARCH" .   ?r)
                      ("INVESTMENT" . ?i)
                      (:endgroup)))

(setq org-capture-templates
      '(("t" "todo" entry (file "~/org/refile.org")
         "* TODO %?\n\n")))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 9)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

;;; clocking

;; Resume clocking task when emacs is restarted
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot    . t)
   (python . t)
   (js     . t)
   (ocaml  . t)
   (java   . t)
   (C    . t)))

;;; disable confirmation of evaluation of code. CAREFUL WHEN EVALUATING
;;; FOREIGN ORG FILES!

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot-mode))


;; overloads to allow double definitions in table formulas

(defun org-table-get-stored-formulas (&optional noerror)
  "Return an alist with the stored formulas directly after current table."
  (interactive) ;; FIXME interactive?
  (let ((case-fold-search t) scol eq eq-alist strings string seen)
    (save-excursion
      (goto-char (org-table-end))
      (when (looking-at "\\([ \t]*\n\\)*[ \t]*#\\+tblfm: *\\(.*\\)")
	(setq strings (org-split-string (org-match-string-no-properties 2)
					" *:: *"))
	(while (setq string (pop strings))
	  (when (string-match "\\`\\(@[-+I<>0-9.$@]+\\|@?[0-9]+\\|\\$\\([a-zA-Z0-9]+\\|[<>]+\\)\\) *= *\\(.*[^ \t]\\)" string)
	    (setq scol (if (match-end 2)
			   (match-string 2 string)
			 (match-string 1 string))
		  scol (if (member (string-to-char scol) '(?< ?>))
			   (concat "$" scol) scol)
		  eq (match-string 3 string)
		  eq-alist (cons (cons scol eq) eq-alist))
            (push scol seen)))))
    (nreverse eq-alist)))

(defun org-table-fedit-finish (&optional arg)
  "Parse the buffer for formula definitions and install them.
With prefix ARG, apply the new formulas to the table."
  (interactive "P")
  (org-table-remove-rectangle-highlight)
  (if org-table-use-standard-references
      (progn
	(org-table-fedit-convert-buffer 'org-table-convert-refs-to-rc)
	(setq org-table-buffer-is-an nil)))
  (let ((pos org-pos) (sel-win org-selected-window) eql var form)
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(@[-+I<>0-9.$@]+\\|@?[0-9]+\\|\\$\\([a-zA-Z0-9]+\\|[<>]+\\)\\) *= *\\(.*\\(\n[ \t]+.*$\\)*\\)"
	    nil t)
      (setq var (if (match-end 2) (match-string 2) (match-string 1))
	    form (match-string 3))
      (setq form (org-trim form))
      (when (not (equal form ""))
	(while (string-match "[ \t]*\n[ \t]*" form)
	  (setq form (replace-match " " t t form)))
	(push (cons var form) eql)))
    (setq org-pos nil)
    (set-window-configuration org-window-configuration)
    (select-window sel-win)
    (goto-char pos)
    (unless (org-at-table-p)
      (user-error "Lost table position - cannot install formulas"))
    (org-table-store-formulas eql)
    (move-marker pos nil)
    (kill-buffer "*Edit Formulas*")
    (if arg
	(org-table-recalculate 'all)
      (message "New formulas installed - press C-u C-c C-c to apply."))))
