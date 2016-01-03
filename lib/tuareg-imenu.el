;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tuareg-imenu.el - support for imenu for the tuareg-mode in Emacs 20.xx
;; tuareg-imenu version 1.2

;;; This file is not part of GNU Emacs.
;;; This file is not part of tuareg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Copyright © 1999 2005 Remi Vanicat, all rights reserved.
;;         Copying is covered by the GNU General Public License.

;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

;; build thanks to the full Ocaml Gramar for ocaml 2.04 by Hendrik Tews
;; http://wwwtcs.inf.tu-dresden.de/~tews/htmlman-2.04/full-grammar.html

;; bug, comment,... to: vanicat@debian.org

;; work with GNU Emacs > 20 and tuareg, but not tested with older emacs,
;; if it work, please report it
;; i haven't try with xemacs (imenu is say to don't work with xemacs,
;; perhaps it is a good occasion for you to try GNU Emacs!)
;; i suggest you also use speedbar (see
;; http://www.ultranet.com/~zappo/speedbar.shtml)


;; to install :
;; put this file in a directory of your load-path
;; add to your .emacs :
;; (autoload 'tuareg-imenu-set-imenu "tuareg-imenu" "Configuration of imenu for tuareg" t)
;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
;;
;; if you want an index menu before
;; (add-hook 'tuareg-imenu-set-imenu ...)
;; add
;; (add-hook 'tuareg-mode-hook 'imenu-add-menubar-index)
;; (may not work, depend (of what? i don't know))

;; to use:
;; - by a gui menu: the imenu-add-menubar-index function add a menu of all
;;   the definition of the current file
;; - by the text menu: M-x imenu show the same menu, but in the minibuffer,
;;   whith completion (just try, it's difficult to explain, easy to
;;   understand)
;; - by the speedbar: refer to their documentation, i have nothing to add
;;   (i suggest define their "F3" binding, and to use it, if like me, you
;;    don't like the mouse)

;; Request for Comment :
;; It's really parsing the OCaml file, maybe it is to slow? (work fine on the
;; bi-pentium 450 of my university)
;;
;; TODO
;; treat val instead of let as top item in module signature
;; the same for method in class

(provide 'tuareg-imenu)

(defconst tuareg-imenu-keyword-regexp
  "\\<\\(module\\(\\s-+type\\)?\\|let\\|type\\|val\\|class\\(\\s-+type\\)?\\|begin\\|and\\|in\\|end\\|method\\|object\\|struct\\|sig\\|external\\|with\\)\\>\\|(\\*\\|\""
  "regexp use to find keyword that have to be handle in the main loop")


(defconst tuareg-imenu-have-end-or-end "\\<\\(begin\\|struct\\|sig\\|object\\|end\\)\\>\\|(\\*\\|\""
  "regexp for keyword that do have an associate \"end\", and \"end\" itself")

(defconst tuareg-imenu-bind-skip-regexp
  "\\s-+\\(\\(rec\\|mutable\\|private\\|virtual\\)\\>\\|'[A-Za-z\300-\377][0-9_'A-Za-z\300-\377]*\\|('.*)\\|\\[.*\\]\\)"
  "Regexp matching stuff to ignore after a binding keyword.")


(defconst tuareg-imenu-function-alist
  '(("^module$" "module" . tuareg-imenu-create-index-bind)
    ("^module\\s-+type$" "module type" . tuareg-imenu-create-index-bind)
    ("^external$" "external" . tuareg-imenu-create-index-bind)
    ("^begin$" "begin" . tuareg-imenu-create-index-ignore-to-end)
    ("^(\\*$" "(*" . tuareg-imenu-create-index-ignore-comment)
    ("^\"$" "\"" . tuareg-imenu-create-index-ignore-string)
    ("^let$" "let" . tuareg-imenu-create-index-bind)
    ("^type$" "type" . tuareg-imenu-create-index-bind)
    ("^object$" "object" . tuareg-imenu-create-index-nested)
    ("^struct$" "struct" . tuareg-imenu-create-index-nested)
    ("^sig$" "sig" . tuareg-imenu-create-index-nested)
    ("^val$" "val" . tuareg-imenu-create-index-bind)
    ("^method$" "method" . tuareg-imenu-create-index-bind)
    ("^class$" "class" . tuareg-imenu-create-index-bind)
    ("^class\\s-+type$" "class type" . tuareg-imenu-create-index-bind)
    ("^and$" "and" . tuareg-imenu-create-index-and)
    ("^with$" "with" . tuareg-imenu-create-index-ignore-constraint))
  "what to do when an item is find,
other keyword are treated separatly")

(defvar tuareg-imenu-syntax-table
  (copy-syntax-table tuareg-mode-syntax-table))

(modify-syntax-entry ?_ "w" tuareg-imenu-syntax-table)

(defun tuareg-imenu-create-index-function ()
  "look for ocaml definition"
  (goto-char (point-min))
  (with-syntax-table
      tuareg-imenu-syntax-table
    (tuareg-imenu-look-nesteed "let")))

(defun tuareg-imenu-look-nesteed  (top-def)
  (let ((alist-list ())
	(buf ())
	(end 1)
	fun)
    (while
	(and (/= end 0)
	     (search-forward-regexp tuareg-imenu-keyword-regexp
				    () 't ()))
;      (setq my-point (cons (point) my-point))
      (setq buf (buffer-substring-no-properties
		 (match-beginning 0)
		 (match-end 0)))
      (cond
       ((setq fun (assoc-default buf
				 tuareg-imenu-function-alist
				 #'string-match))
	(setq alist-list (apply (cdr fun)
				(list (car fun) alist-list))))
       ((string= buf "in")
	(if alist-list
	    (setq alist-list (cdr alist-list))))
       ((string= buf "end")
	(setq end (1- end)))
       ((string= buf "begin")
	(setq end (1+ end)))))
    (tuareg-imenu-recolect-info alist-list top-def)))


(defun tuareg-imenu-create-index-bind (type alist-list)
  "look for bind form"
  (while (looking-at tuareg-imenu-bind-skip-regexp)
    (goto-char (match-end 0)))
  (if (search-forward-regexp "\\<\\sw+\\>" () 't ())
      (if (string= (buffer-substring-no-properties
		    (match-beginning 0)
		    (match-end 0))
		   "_")
	  alist-list
	(cons (list type (cons (buffer-substring-no-properties
				(match-beginning 0)
				(match-end 0))
			       (match-beginning 0)))
	      alist-list))
    ()))


(defun tuareg-imenu-create-index-ignore-to-end (type alist-list)
  (let ((cont 1)
	buf)
    (while (and (/= cont 0)
		(search-forward-regexp tuareg-imenu-have-end-or-end () 't ()))
      (setq buf (buffer-substring-no-properties (match-beginning 0)
						(match-end 0)))
      (cond ((string= buf "end")
	     (setq cont (1- cont)))
	    ((string= buf "\"")
	     (tuareg-imenu-create-index-ignore-string () ()))
	    ((string= buf "(*")
	     (tuareg-imenu-create-index-ignore-comment () ()))
	    ('t
	     (setq cont (1+ cont))))))
  alist-list)

(defun tuareg-imenu-create-index-ignore-comment (type alist-list)
  (let ((cont 1)
	buf)
    (while (and (/= cont 0)
		(search-forward-regexp "(\\*\\|\\*)\\|\"" () 't ()))
      (setq buf (buffer-substring-no-properties (match-beginning 0)
						(match-end 0)))
      (cond ((string= buf "(*")
	     (setq cont (1+ cont)))
	    ((string= buf "*)")
	     (setq cont (1- cont)))
	    ((string= buf "\"")
	     (tuareg-imenu-create-index-ignore-string () ())))))
  alist-list)

(defun tuareg-imenu-create-index-ignore-string (type alist-list)
  (let ((cont t)
	buf)
    (while (and cont
		(search-forward-regexp "\\\\\"\\|\"" () 't ()))
      (setq buf (buffer-substring-no-properties (match-beginning 0)
						(match-end 0)))
      (cond ((string= buf "\"")
	     (setq cont ())))))
  alist-list)

(defun tuareg-imenu-create-index-nested (type alist-list)
  (let ((type (car (car alist-list)))
	(cell (car (cdr (car alist-list))))
	top-def)
    (setq top-def
	  (cond ((string= type "class") "method")
		((string= type "class type") "method")
		((string= type "module") "let")
		((string= type "module type") "val")))
    (if (and top-def
	     (number-or-marker-p (cdr cell)))
	(progn
	  (setcdr cell
		  (cons (cons "definition" (cdr cell))
			(tuareg-imenu-look-nesteed top-def)))
	  alist-list)
      (tuareg-imenu-create-index-ignore-to-end type alist-list))))

(defun tuareg-imenu-create-index-and (type alist-list)
  (if (looking-at "\\s-+\\(type\\|module\\)\\>")
      (progn
	(goto-char (match-end 0))
	alist-list)
    ;; i haven't verify the gramar here
    (while (looking-at tuareg-imenu-bind-skip-regexp)
      (goto-char (match-end 0)))
    ;; what to do when alist-list is empty? i do supose it not worth any
    ;; effort (FIXME)
    (if (and alist-list
	     (search-forward-regexp "\\<\\sw+\\>" () 't ()))
	(cons (list* (car (car alist-list))
		     (cons (buffer-substring-no-properties (match-beginning 0)
							   (match-end 0))
			   (match-beginning 0))
		     (cdr (car alist-list)))
	      (cdr alist-list))
      ())))

(defun tuareg-imenu-create-index-ignore-constraint (type alist-list)
  (if (looking-at "\\s-+\\(type\\|module\\)\\>")
      (goto-char (match-end 0)))
  alist-list)


(defun tuareg-imenu-recolect-info (alist-list top-def)
  (let (type final-alist tmp tmp-list
	(sorted-alist (sort alist-list
			   (lambda (x y)
			     (cond ((string= (car x) top-def) ())
				   ((string= (car y) top-def) 't)
				   ('t (string< (car y) (car x))))))))
    (while sorted-alist
      (setq type (car (car sorted-alist)))
      (setq tmp-list (cdr (car sorted-alist)))
      (setq sorted-alist (cdr sorted-alist))
      (while (string= type (car (car sorted-alist)))
	(setq tmp-list (cons (car (cdr (car sorted-alist))) tmp-list))
	(setq sorted-alist (cdr sorted-alist)))
      (setq final-alist (cons (cons type tmp-list) final-alist)))
    (setq tmp (mapcar (lambda (x)
			(cons (car x)
			      (sort (cdr x)
				    (lambda (x y)
				      (string< (car x) (car y))))))
		      final-alist))
    (if (string= (car (car tmp)) top-def)
	(append (cdr (car tmp)) (cdr tmp))
      tmp)))


;;;###autoload
(defun tuareg-imenu-set-imenu ()
  "setup imenu for tuareg"
  (interactive)
  (setq imenu-create-index-function 'tuareg-imenu-create-index-function))
;  (imenu-add-menubar-index))
