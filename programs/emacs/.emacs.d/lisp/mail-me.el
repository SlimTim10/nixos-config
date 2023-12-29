;;; mail-me.el --- Send an email to myself.
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 9 May 2014

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

;; Send an email to myself.

(defun mail-me (mail-subject mail-body)
  "Send an email to myself, given MAIL-SUBJECT and MAIL-BODY."
  (interactive)
  (with-temp-buffer
	(insert "From: Tim
To: slimtim10@gmail.com
Subject: ")
	(insert mail-subject)
	(insert "
--text follows this line--
")
	(insert mail-body)
	(mail-send)))

(provide 'mail-me)
