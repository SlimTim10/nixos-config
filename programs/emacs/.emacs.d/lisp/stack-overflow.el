;;; stack-overflow.el --- Open Google's top stack overflow result.
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 27 Mar 2014

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

;; Open Google's top stack overflow result.

(defun stack-overflow ()
  "Open Google's top stack overflow result of the selected region if any, otherwise display a query prompt."
  (interactive)
  (save-excursion
	(setq so-url
		  (concat
		   "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
		   "stack%20overflow%20"
		   (url-hexify-string
			(if mark-active
				(buffer-substring (region-beginning) (region-end))
			  (read-string "Stack overflow: ")))))
	(switch-to-buffer (url-retrieve-synchronously so-url))
	(goto-char (point-min))
	(search-forward "<div id=\"res\"" nil t)
	(search-forward "/url?q=" nil t)
	(setq beg (point))
	(search-forward "&amp;" nil t)
	(copy-region-as-kill beg (match-beginning 0))
	(kill-buffer (current-buffer)))
  (browse-url (current-kill 1)))

(provide 'stack-overflow)
