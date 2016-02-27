;;; backup.el --- control emacs backup               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Eduardo Bellani

;; Author: Eduardo Bellani <ebellani@gmail.com>
;; Keywords: files

;;; Code:

;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/


(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
            `((".*" ,temporary-file-directory t)))

;;; backup.el ends here
