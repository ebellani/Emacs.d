;;; eshell-functions.el --- convenience functions for using in eshell

;; Copyright (C) 2011  Eduardo Bellani

;; Author: Eduardo Bellani <b-man@agora>
;; Keywords: lisp, terminals, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Check out examples here http://www.emacswiki.org/emacs/EshellFunctions

;;; Code:
(defgroup eshell-bellani nil
  "Contains eshell modifications by Eduardo Bellani (ebellani@gmail.com)"
  :tag "Eshell Bellani"
  :group 'eshell-module)

(defun eshell-maybe-bol ()
  "I use the following code. It makes C-a go to the beginning of
the command line, unless it is already there, in which case it
goes to the beginning of the line. So if you are at the end of
the command line and want to go to the real beginning of line,
hit C-a twice:"
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(add-hook 'eshell-mode-hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

(defun eval-string-and-wait (a-string)
  (eshell/wait (eshell-eval (eshell-parse-command a-string))))

(defun show-it ()
  #'(lambda (f)
      (eshell-eval (eshell-parse-command
                    (format "%s %s.pdf" pdfviewer f)))))

(defun pdflatex-simple ()
  "Simple pdflatex execution"
  #'(lambda (f)
      (eshell-evaln (eshell-parse-command
                     (format "pdflatex -interaction batchmode %s.tex" f)))))

(defun lt-complex (filename func &optional pdfviewer)
  "Uses pdf latex in batchmode for the FILENAME and removes
everything that is created by pdflatex but the log file and the
pdf. If there is no PDFVIEWER, uses the system default."
  (unless pdfviewer (setf pdfviewer "evince"))
  (funcall func filename)
  (mapc #'eshell/rm (directory-files "." t (format "%s\\.\\(out\\|blg\\|aux\\)" filename))))

(defun lt-simple (filename &optional pdfviewer)
  (lt-complex filename
              #'(lambda (f)
                  (eshell/wait
                   (funcall (pdflatex-simple) f))
                  (funcall  (show-it) f))
              pdfviewer))

(defun lt-bibtex (filename &optional pdfviewer)
  (lt-complex filename
              #'(lambda (f)
                  (eshell/wait
                   (funcall (pdflatex-simple) f))
                  (eshell/wait
                   (eshell-evaln (eshell-parse-command
                                  (format "bibtex %s.aux" f))))
                  (eshell/wait
                   (funcall (pdflatex-simple) f))
                  (funcall (show-it) f))
              pdfviewer))

(defun c ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


(defun git-delete-branch (branch-name)
  "Deletes the git branch, both locally and on origin. Used to
clean things up when a project gets too messy."
  (eval-string-and-wait (format "git push origin :%s" branch-name))
  (eval-string-and-wait (format "git br -D %s" branch-name)))

(defun git-delete-branches! (&rest branches)
  "Delete all branches passed in a string list. "
  (interactive)
  (dolist (branch branches)
    (git-delete-branch branch))
  nil)

(provide 'em-bellani)
;;; eshell-functions.el ends here
