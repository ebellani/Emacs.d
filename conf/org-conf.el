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

;; The following, as its dependent on absolute file paths, are commented out

;; set the default org directory

(setq org-directory (concat *domain-custom* "org/"))
(setq refile-path (concat org-directory "refile.org"))

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
      `(("t" "todo" entry (file ,refile-path)
         "* TODO %?\n\n")))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 9)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

;;; clocking

(setq org-clock-persist-file (concat *domain-custom* "org-clock-save.el"))

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

(setq org-src-fontify-natively t)

(add-to-list 'org-src-lang-modes (quote ("racket" . scheme)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot    . t)
   (python . t)
   (js     . t)
   (ocaml  . t)
   (java   . t)
   (scheme . t)
   (C      . t)
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
