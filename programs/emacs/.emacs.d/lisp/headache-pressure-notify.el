;;; headache-pressure-notify.el --- Send an alert by email when the outdoor pressure may cause a headache.
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 22 May 2014

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar *headache-pressure-timer* nil)

;; Get the pressure from http://weather.gc.ca/city/pages/on-143_metric_e.html, analyze it, and send an email alert if necessary.
;; An alert will be sent if the pressure is below 101.5 or if the pressure is 101.5 - 101.7 and falling.
;; Note: this function uses mail-me

(defun headache-pressure-notify ()
  (save-excursion
	(switch-to-buffer (url-retrieve-synchronously "http://weather.gc.ca/city/pages/on-143_metric_e.html"))
	(goto-char (point-min))
	(search-forward "<dt>Pressure:</dt>" nil t)
	(re-search-forward "<dd>\\(.*?\\)&nbsp;<" nil t)
	(setq s-pressure (match-string-no-properties 1))
	(setq n-pressure (string-to-number s-pressure))
	(search-forward "<dt>Tendency:</dt>" nil t)
	(re-search-forward "<dd>\\(.*?\\)<\/dd>" nil t)
	(setq tendency (match-string-no-properties 1))
	(kill-buffer (current-buffer))
	)
  (cond
   ((< n-pressure 101.5)
	(let ((subj "Headache Alert")
		  (msg (concat "Probable headache approaching!\n\nThe pressure is currently " s-pressure " and " tendency ".")))
	  (mail-me subj msg)
	  (my-mail-to "brittanyedwards@rogers.com" subj msg)
	  (my-mail-to "jean@jeanpare.net" subj msg)
	  (my-mail-to "seasidecity@gmail.com" subj msg)))
   ((and (<= n-pressure 101.7) (string= tendency "falling"))
	(let ((subj "Headache Alert")
		  (msg (concat "Possible headache approaching.\n\nThe pressure is currently " s-pressure " and " tendency ".")))
	  (mail-me subj msg)
	  (my-mail-to "brittanyedwards@rogers.com" subj msg)
	  (my-mail-to "jean@jeanpare.net" subj msg)
	  (my-mail-to "seasidecity@gmail.com" subj msg)))
   ))

;; Start checking every 4 hours

(defun headache-pressure-notify-start ()
  (interactive)
  (setq *headache-pressure-timer* (run-at-time "12:00am" (* 3600 4) 'headache-pressure-notify))
)

;; Stop

(defun headache-pressure-notify-stop ()
  (interactive)
  (cancel-timer *headache-pressure-timer*))

(provide 'headache-pressure-notify)
