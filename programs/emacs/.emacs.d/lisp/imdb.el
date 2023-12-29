;;; imdb.el --- Use Google's "I'm feeling lucky" to go to an IMDB page
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 16 Apr 2014

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

;; Use Google's "I'm feeling lucky" to go to an IMDB page

(defun imdb ()
  "Go to the IMDB page of the selected region if any, otherwise display a query prompt."
  (interactive)
  (browse-url
   (concat
	"http://www.google.com/search?ie=utf-8&oe=utf-8&btnI&q=imdb "
	(url-hexify-string
	 (if mark-active
		 (buffer-substring (region-beginning) (region-end))
	   (read-string "IMDB: "))))))

(provide 'imdb)
