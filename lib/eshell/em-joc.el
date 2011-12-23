;;; **************************************************************************
;; @(#) em-joc.el -- eshell extensions
;; @(#) $Id: em-joc.el,v 1.5 2001/01/01 22:00:04 root Exp $

;; This file is not part of Emacs

;; Copyright (C) 2000-2001 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         September 10, 2000
;; Latest Version:  http://www.northbound-train.com/emacs.html

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;; **************************************************************************

;;; Description:
;;
;;  This package provides extensions to John Wiegley's eshell
;;  <http://www.gci-net.com/users/j/johnw/eshell.html>

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add the line:
;;
;;     (load "em-joc")
;;
;;  somewhere in your .emacs file.
;;
;;  To use any of the eshell "built-in" functions:
;;  1. Add the module "eshell-joc" to the eshell Modules List:
;;
;;     M-x customize-group [RET] eshell-module [RET]
;;
;;  2. Resart eshell
;;
;;  For all other functions, see the documentation below.

;;; Built-in Functions:
;;
;;  o clear - works like the Unix clear or DOS cls command.

;;; Other Functions:
;;
;;  o joc-eshell-prompt - replaces the default eshell prompt.  This prompt will
;;    replace your home directory in the prompt string with a tilde (~).  It
;;    also pre-pends a newline, and encloses the whole thing in angle brackets.
;;    It retains the use of `$' and `#'.  Example:
;;
;;       <~/mail> $
;;
;;    To use this function, customize the eshell-prompt-function:
;;
;;       M-x customize-option [RET] eshell-prompt-function [RET]
;;
;;    You can also turn off the pre-pend newline feature; see the
;;    customizations section below.
;;
;;  o joc-remote-eshell-cmd - send eshell a command from somewhere else.  The
;;    command will appear at the prompt and be entered into the command history
;;    as if you had entered it normally.
;;
;;    Examples:
;;
;;    (global-set-key [(control f2)]  (function
;;        (lambda () "change eshell's directory to current buffer's default-directory"
;;          (interactive)
;;          (let ((the-dir default-directory)
;;                (start 0))
;;            (while (string-match " " the-dir start)
;;              (setq the-dir (replace-match "\\ " nil t the-dir))
;;              (setq start (+ (match-end 0) 1)))
;;            (joc-remote-eshell-cmd (concat "cd " the-dir) 1)))))
;;
;;    (global-set-key [(shift f2)] (function
;;       (lambda (reg-start reg-end)
;;          "send region to eshell as command input"
;;          (interactive "r")
;;          (let ((cmd (buffer-substring reg-start reg-end)))
;;            (joc-remote-eshell-cmd cmd 1)))))
;;
;;    Optional arguments: create-or-warn no-newline no-switch eshell-buff-name

;;; Customization:
;;
;;  M-x `joc-eshell' to customize all package options.
;;
;;  The following variables can be customized:
;;
;;  o `joc-eshell-prompt-newline'
;;     If non-nil, joc-eshell-prompt will pre-pend a newline before the prompt
;;     string.

;;; To Do:
;;
;;  o Nothing, at the moment.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of dired-single was developed and tested with NTEmacs 20.5.1
;;  and 2.7 under Windows NT 4.0 SP6 and Emacs 20.7.1 under Linux (RH7).
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:

;;; **************************************************************************
;;; ***** customization routines
;;; **************************************************************************
(defgroup eshell-joc nil
  "Contains eshell modifications by Joe Casadonte (emacs@northbound-train.com)"
  :tag "Eshell JOC"
  :group 'eshell-module)

;; ---------------------------------------------------------------------------
(defcustom joc-eshell-prompt-newline t
  "If non-nil, joc-eshell-prompt will pre-pend a newline before the prompt
string."
  :group 'eshell-joc
  :type 'boolean)

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst em-joc-version
  "$Revision: 1.5 $"
  "em-joc version number.")

;; ---------------------------------------------------------------------------
(defun em-joc-version-number ()
  "Returns em-joc version number."
  (string-match "[0123456789.]+" em-joc-version)
  (match-string 0 em-joc-version))

;; ---------------------------------------------------------------------------
(defun em-joc-display-version ()
  "Displays em-joc version."
  (interactive)
  (message "em-joc version <%s>." (em-joc-version-number)))

;;; **************************************************************************
;;; ***** built-in functions
;;; **************************************************************************
(defun eshell/clear ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
	;; simply delete the region
	(delete-region (point-min) (point-max))))

;;; **************************************************************************
;;; ***** non-built-in functions
;;; **************************************************************************
(defun joc-eshell-prompt ()
  "When spitting out the prompt, substitute a tilde (~) for the user's HOME directory."
  (let ((prompt (eshell/pwd))
		(home-dir (expand-file-name "~")))
	;; get rid of the nasty backslashes
	(while (string-match "\\\\" home-dir)
	  (setq home-dir (replace-match "/" nil t home-dir)))
	;; match home-dir at the begining of the line
	;; be careful not to match `/users/foo.old' if $HOME is `/users/foo'
	(if (and (string-match (concat "^\\(" home-dir "\\)\\(/.*\\)?$") prompt))
		(setq prompt (replace-match
					  (if (and (match-string 2)) "~\\2"	"~")
					  nil nil prompt 0)))
	;; return the prompt
	;;   -put a newline in front of it, first (maybe)
	;;   -then add angle brackets around it
	;;   -tack on $ or # depending on user id
	(concat (if joc-eshell-prompt-newline "\n")
			"<" prompt "> "
			(if (= (user-uid) 0) " # " " $ "))))

;;; **************************************************************************
(defun joc-remote-eshell-cmd (cmd &optional create-or-warn no-newline no-switch the-buffer-name)
  "Send remote command to eshell.

If there is text already present on the current line in the eshell, it will be
commented out with a hash (#) and entered into the command history before
the new command is inserted.

CREATE-OR-WARN determines what happens if there is no current eshell process.
A value of 0 will report an error, -1 will fail silently, and 1 will create
a new process (default: 0).
If NO-NEWLINE is `t' it will send the command to the eshell, but not execute
it (default: nil).
If NO-SWITCH iss `t' it will not switch to the eshell buffer (default: nil).
THE-BUFFER-NAME is the name of the buffer to look for eshell in (defaults to
eshell-buffer-name)."
  (interactive "s")

  ;; init values
  (if (not create-or-warn)
	  (setq create-or-warn 0))

  ;; make sure eshell is loaded
  (if (not (featurep 'eshell))
	  (load "eshell" nil t))

  (let* ((current (current-buffer))
		 (buff-name (or the-buffer-name eshell-buffer-name))
		 (buff (get-buffer buff-name)))

	;; do we need to create a process if one doesn't exist?
	(if (and (not buff) (= create-or-warn 1))
		(progn
		  (save-excursion (eshell))
		  (setq buff (get-buffer buff-name))))

	;; switch to eshell buffer or report error
	(if (not buff)
		(if (/= create-or-warn -1)
			(error "Buffer <%s> not found" buff-name))
	  (switch-to-buffer buff)

	  ;; kill anything on current line
	  (let ((here (point)) bol eol)
		(end-of-line)
		(setq eol (point))
		(eshell-bol)
		(setq bol (point))

		(if (or (/= here bol)
				(/= here eol))
			(progn
			  (insert-and-inherit "#")
			  (eshell-reset))))

	  ;; this is essentially (eshell-insert-command x)
	  (if eshell-last-output-end
		  (goto-char eshell-last-output-end))
	  (insert-and-inherit cmd)

	  ;; don't send newline if asked not to
	  (if (not no-newline)
		  (funcall 'eshell-send-input))))

  ;; return to previous buffer if asked to
  (if no-switch
	  (switch-to-buffer current)))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'em-joc)

;; em-joc.el ends here!
;;; **************************************************************************
;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************
