(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-directory "~/Data/org/")

(setq org-agenda-files '("~/Data/org/tasks.org" "~/Data/org/bills.org"))
(setq org-capture-templates
      '(("b" "Bills to pay" entry (file+headline "~/Data/org/bills.org" "Bills") "* TODO Pay %?
")
        ("t" "Tasks" entry (file+headline "~/Data/org/tasks.org" "Tasks")
         "* TODO %? %i")))
(setq org-refile-targets '((org-agenda-files :level . 1)))
(setq org-todo-keywords '((sequence "TODO" "DONE(!)")))

;; (setq org-default-notes-file (concat org-directory "/notes.org"))
