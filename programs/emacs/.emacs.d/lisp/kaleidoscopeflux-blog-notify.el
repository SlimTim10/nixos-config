;;; kaleidoscopeflux-blog-notify.el --- Notify me when the kaleidoscope flux blog has a new post.
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 12 May 2014

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

(defvar *last-post-title* nil)
(defvar *kaleidoscopeflux-blog-timer* nil)

(defun kaleidoscopeflux-blog-notify ()
  (setq post-title nil)
  (save-excursion
	(switch-to-buffer (url-retrieve-synchronously "http://kaleidoscopeflux.blogspot.ca/feeds/posts/default?alt=rss"))
	(goto-char (point-min))
	(search-forward "<title>Kaleidoscope Flux</title>" nil t)
	(re-search-forward "<title>\\(.*?\\)<\/title>" nil t)
	(setq post-title (match-string-no-properties 1))
	(re-search-forward "<link>\\(.*?\\)<\/link>" nil t)
	(setq post-link (match-string-no-properties 1))
	(kill-buffer (current-buffer))
	)
  (when (not (equal *last-post-title* post-title))
	(mail-me (concat "New kaleidoscopeflux post: " post-title) post-link)
	(setq *last-post-title* post-title)))

(defun kaleidoscopeflux-blog-notify-start ()
  (interactive)
  (setq *kaleidoscopeflux-blog-timer* (run-at-time "12:00am" (* 3600 24) 'kaleidoscopeflux-blog-notify))
)

(defun kaleidoscopeflux-blog-notify-stop ()
  (interactive)
  (cancel-timer *kaleidoscopeflux-blog-timer*))

(provide 'kaleidoscopeflux-blog-notify)
