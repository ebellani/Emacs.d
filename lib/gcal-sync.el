;;; gcal-sync.el --- import google calendar into our dairy  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Steve Beaulac

;; Author: Steve Beaulac <steve@sagacity.ca>
;; Keywords:

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
;;;
;;; Add the folling line to your diary file #include "~/.emacs.d/gcal-diary"
;;; This is slightly modified from
;;; https://gist.githubusercontent.com/SjB/18999655439765eb413a29c6aedc9385/raw/162ab1a7ed5b22186712c8a4bb7d46a01d0985b0/gcal-sync.el,
;;; making it parameterized over cretentials. I did that to help fetching
;;; credentials from .authinfo.gpg instead of hard coding custom.el or similar
;;; things. -- E.B.
;;

;;; Code:

(defun gcal-sync--import-ical-to-diary (diary)
  "Import an ical buffer to the diary file"
  (async-start
   `(lambda ()
      (with-temp-buffer
        (set-buffer-multibyte t)
        (set-buffer-file-coding-system 'utf-8)
        (insert ,(buffer-substring-no-properties
                  (point-min)
                  (point-max)))
        (icalendar-import-buffer ,diary t)))
   (lambda (results)
     (message "Ical turned to diary %s %s."
              diary
              (if results
                  "successfully"
                "unsuccessfully")))))

(defun gcal-sync--import-to-diary-file (url diary)
  "Fetch a ical calendar from a private google calendar url
and import it into a diary file"
  (url-retrieve url (lambda (status)
		       (when (plist-get status :error)
			 (cl-destructuring-bind (error-symbol . data)
			     (plist-get status :error)
			   (signal error-symbol data)))
		       (gcal-sync--import-ical-to-diary diary))))

(defun gcal-build-url (credentials)
  "Build a standard (as of 2022-08-15) gcal ics url from
a (username . password) `CREDENTIALS' pair"
  (format "https://calendar.google.com/calendar/ical/%s/private-%s/basic.ics"
          (cl-first credentials) (cl-second credentials)))

(defun gcal-sync-calendars-to-diary (credentials-list gcal-diary-file)
  "Fetch several google calendar (using a list of username/password
pairs) and import them into a default diary file"
  (interactive)
  (let ((urls (mapcar #'gcal-build-url credentials-list)))
    (with-temp-file gcal-diary-file (erase-buffer))
    (while urls
      (gcal-sync--import-to-diary-file (car urls) gcal-diary-file)
      (setq urls (cdr urls)))))

(provide 'gcal-sync)
;;; gcal-sync.el ends here
