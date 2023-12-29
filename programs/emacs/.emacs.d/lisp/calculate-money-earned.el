;;; calculate.money-earned.el --- Calculate money earned based on hours worked and hourly wage

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

(defun calculate-money-earned ()
  "Calculate money earned given hours worked (hh:mm) and hourly wage."
  (interactive)
  (let* ((hours-worked (read-string "Hours worked (hh:mm): "))
		 (wage (string-to-number (read-string "Hourly wage: ")))
		 (hours-decimal (+ (string-to-number (car (split-string hours-worked ":")))
						   (/ (string-to-number (cadr (split-string hours-worked ":"))) 60.0)))
		 (money-earned (* hours-decimal wage)))
	(message (format "$%.2f" money-earned))))

(provide 'calculate-money-earned)
