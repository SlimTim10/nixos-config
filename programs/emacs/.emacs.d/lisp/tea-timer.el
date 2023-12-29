;;; tea-timer.el --- Start a tea timer that uses a visual alert (press Enter after alert)
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 3 Apr 2014

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

(defun my-alarm ()
  (interactive)
  (setq frame-name (cdr (assoc 'name (frame-parameters))))
  (set-frame-name "ALARM!")
  (dotimes (i 30)
	(beep))
  (read-from-minibuffer "ALARM!")
  (set-frame-name frame-name))

(defun tea-timer ()
  (interactive)
  (setq input-string (read-string "Tea timer: "))
  (if (eq (string-match "\\([0-9]+\\)\s*min" input-string) 0)
	  (setq sec (* (string-to-number (match-string 1 input-string)) 60))
	(setq sec (string-to-number input-string)))
  (run-with-timer sec nil 'my-alarm))

(provide 'tea-timer)
