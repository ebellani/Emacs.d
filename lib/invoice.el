;;; invoice.el --- Utilities for handling invoices

;; Copyright (C) 2013  Eduardo Bellani

;; Author: Eduardo Bellani <b-man@agora>
;; Keywords: tex, calendar

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

;;; Code:

(require 'calendar)

(setq calendar-date-display-form calendar-european-date-display-form)

;;

(defun insert-month-invoices (format-string starting-day month year)
  "TODO: make it from, upto. Usage example:
  (insert-month-invoices \"\Fee{%s development hour} {50.00} {4}\" 1 10 2013)"
  (loop for day from starting-day upto (calendar-last-day-of-month month year)
        if (let ((work-day (calendar-day-of-week (list month day year))))
             (and (not (= work-day 0))
                  (not (= work-day 6))))
        do
        (progn (insert (format format-string (calendar-date-string
                                              (list month day year))))
               (newline))))

(provide 'insert-month-invoices)
;;; invoice.el ends here
