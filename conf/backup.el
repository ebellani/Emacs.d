;;; backup.el --- control emacs backup               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Eduardo Bellani

;; Author: Eduardo Bellani <ebellani@gmail.com>
;; Keywords: files

;;; Code:

;; https://www.emacswiki.org/emacs/BackupDirectory

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(provide 'backup)
;;; backup.el ends here
