;;; weather.el --- Get the weather from http://weather.gc.ca/city/pages/on-143_metric_e.html and output it.
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 28 Mar 2014

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

;; Center a string inside spacing of given length

(defun center-string (str len)
  (store-substring (make-string len ?\s) (/ (- len (length str)) 2) str))

;; Get the weather from http://weather.gc.ca/city/pages/on-143_metric_e.html and output it.

(defun weather ()
  "Output the weather as a message."
  (interactive)
  (save-excursion
	(switch-to-buffer (url-retrieve-synchronously "http://weather.gc.ca/city/pages/on-143_metric_e.html"))
	(goto-char (point-min))
	(search-forward "<div id=\"currentcond\">" nil t)
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cur-cond (concat (match-string-no-properties 1)))
	(re-search-forward "<p class=\"temperature\">\\(.+\\)&deg;" nil t)
	(setq cur-temp (concat (match-string-no-properties 1) "°C"))
	(search-forward "<div id=\"forecastData\">" nil t)
	(search-forward "<div class=\"fperiod" nil t)
	(re-search-forward "<abbr title=\".+\">\\(.*\\)<\/abbr>" nil t)
	(setq day-1 (match-string-no-properties 1))
	(re-search-forward "<span class=\"fdate\">\\(.*\\)&.*;<.+>\\(.*\\)<" nil t)
	(setq date-1 (concat (match-string-no-properties 1) " " (match-string-no-properties 2)))
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cond-1 (concat (match-string-no-properties 1)))
	(re-search-forward "<li class=\"high\".*>\\(.*\\)&.*;" nil t)
	(setq temp-1 (concat (match-string-no-properties 1) " C"))
	(search-forward "<div class=\"fperiod" nil t)
	(re-search-forward "<abbr title=\".+\">\\(.*\\)<\/abbr>" nil t)
	(setq day-2 (match-string-no-properties 1))
	(re-search-forward "<span class=\"fdate\">\\(.*\\)&.*;<.+>\\(.*\\)<" nil t)
	(setq date-2 (concat (match-string-no-properties 1) " " (match-string-no-properties 2)))
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cond-2 (concat (match-string-no-properties 1)))
	(re-search-forward "<li class=\"high\".*>\\(.*\\)&.*;" nil t)
	(setq temp-2 (concat (match-string-no-properties 1) " C"))
	(search-forward "<div class=\"fperiod" nil t)
	(re-search-forward "<abbr title=\".+\">\\(.*\\)<\/abbr>" nil t)
	(setq day-3 (match-string-no-properties 1))
	(re-search-forward "<span class=\"fdate\">\\(.*\\)&.*;<.+>\\(.*\\)<" nil t)
	(setq date-3 (concat (match-string-no-properties 1) " " (match-string-no-properties 2)))
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cond-3 (concat (match-string-no-properties 1)))
	(re-search-forward "<li class=\"high\".*>\\(.*\\)&.*;" nil t)
	(setq temp-3 (concat (match-string-no-properties 1) " C"))
	(search-forward "<div class=\"fperiod" nil t)
	(re-search-forward "<abbr title=\".+\">\\(.*\\)<\/abbr>" nil t)
	(setq day-4 (match-string-no-properties 1))
	(re-search-forward "<span class=\"fdate\">\\(.*\\)&.*;<.+>\\(.*\\)<" nil t)
	(setq date-4 (concat (match-string-no-properties 1) " " (match-string-no-properties 2)))
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cond-4 (concat (match-string-no-properties 1)))
	(re-search-forward "<li class=\"high\".*>\\(.*\\)&.*;" nil t)
	(setq temp-4 (concat (match-string-no-properties 1) " C"))
	(search-forward "<div class=\"fperiod" nil t)
	(re-search-forward "<abbr title=\".+\">\\(.*\\)<\/abbr>" nil t)
	(setq day-5 (match-string-no-properties 1))
	(re-search-forward "<span class=\"fdate\">\\(.*\\)&.*;<.+>\\(.*\\)<" nil t)
	(setq date-5 (concat (match-string-no-properties 1) " " (match-string-no-properties 2)))
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cond-5 (concat (match-string-no-properties 1)))
	(re-search-forward "<li class=\"high\".*>\\(.*\\)&.*;" nil t)
	(setq temp-5 (concat (match-string-no-properties 1) " C"))
	(search-forward "<div class=\"fperiod" nil t)
	(re-search-forward "<abbr title=\".+\">\\(.*\\)<\/abbr>" nil t)
	(setq day-6 (match-string-no-properties 1))
	(re-search-forward "<span class=\"fdate\">\\(.*\\)&.*;<.+>\\(.*\\)<" nil t)
	(setq date-6 (concat (match-string-no-properties 1) " " (match-string-no-properties 2)))
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cond-6 (concat (match-string-no-properties 1)))
	(re-search-forward "<li class=\"high\".*>\\(.*\\)&.*;" nil t)
	(setq temp-6 (concat (match-string-no-properties 1) " C"))
	(search-forward "<div class=\"fperiod" nil t)
	(re-search-forward "<abbr title=\".+\">\\(.*\\)<\/abbr>" nil t)
	(setq day-7 (match-string-no-properties 1))
	(re-search-forward "<span class=\"fdate\">\\(.*\\)&.*;<.+>\\(.*\\)<" nil t)
	(setq date-7 (concat (match-string-no-properties 1) " " (match-string-no-properties 2)))
	(re-search-forward "<img.*alt=\"\\(.*?\\)\"" nil t)
	(setq cond-7 (concat (match-string-no-properties 1)))
	(re-search-forward "<li class=\"high\".*>\\(.*\\)&.*;" nil t)
	(setq temp-7 (concat (match-string-no-properties 1) " C"))
	(kill-buffer (current-buffer))
	)
  (setq current (propertize "Current" 'face '(:family "Courier New" :height 140)))
  (setq cur-cond (propertize cur-cond 'face '(:family "Courier New" :height 80)))
  (setq cur-temp (propertize cur-temp 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq day-width 24)
  (setq day-1 (propertize (center-string day-1 day-width) 'face '(:family "Courier New" :height 140)))
  (setq day-2 (propertize (center-string day-2 day-width) 'face '(:family "Courier New" :height 140)))
  (setq day-3 (propertize (center-string day-3 day-width) 'face '(:family "Courier New" :height 140)))
  (setq day-4 (propertize (center-string day-4 day-width) 'face '(:family "Courier New" :height 140)))
  (setq day-5 (propertize (center-string day-5 day-width) 'face '(:family "Courier New" :height 140)))
  (setq day-6 (propertize (center-string day-6 day-width) 'face '(:family "Courier New" :height 140)))
  (setq day-7 (propertize (center-string day-7 day-width) 'face '(:family "Courier New" :height 140)))
  (setq date-width 24)
  (setq date-1 (propertize (center-string date-1 date-width) 'face '(:family "Courier New" :height 140)))
  (setq date-2 (propertize (center-string date-2 date-width) 'face '(:family "Courier New" :height 140)))
  (setq date-3 (propertize (center-string date-3 date-width) 'face '(:family "Courier New" :height 140)))
  (setq date-4 (propertize (center-string date-4 date-width) 'face '(:family "Courier New" :height 140)))
  (setq date-5 (propertize (center-string date-5 date-width) 'face '(:family "Courier New" :height 140)))
  (setq date-6 (propertize (center-string date-6 date-width) 'face '(:family "Courier New" :height 140)))
  (setq date-7 (propertize (center-string date-7 date-width) 'face '(:family "Courier New" :height 140)))
  (setq cond-width 38)
  (setq cond-1 (propertize (center-string cond-1 cond-width) 'face '(:family "Courier New" :height 80)))
  (setq cond-2 (propertize (center-string cond-2 cond-width) 'face '(:family "Courier New" :height 80)))
  (setq cond-3 (propertize (center-string cond-3 cond-width) 'face '(:family "Courier New" :height 80)))
  (setq cond-4 (propertize (center-string cond-4 cond-width) 'face '(:family "Courier New" :height 80)))
  (setq cond-5 (propertize (center-string cond-5 cond-width) 'face '(:family "Courier New" :height 80)))
  (setq cond-6 (propertize (center-string cond-6 cond-width) 'face '(:family "Courier New" :height 80)))
  (setq cond-7 (propertize (center-string cond-7 cond-width) 'face '(:family "Courier New" :height 80)))
  (setq temp-width 24)
  (setq temp-1 (propertize (center-string temp-1 temp-width) 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq temp-2 (propertize (center-string temp-2 temp-width) 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq temp-3 (propertize (center-string temp-3 temp-width) 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq temp-4 (propertize (center-string temp-4 temp-width) 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq temp-5 (propertize (center-string temp-5 temp-width) 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq temp-6 (propertize (center-string temp-6 temp-width) 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq temp-7 (propertize (center-string temp-7 temp-width) 'face '(:family "Courier New" :height 140 :background "white" :foreground "black")))
  (setq weather-string
		(format "%s\n%s\n%s\n\n%s %s %s %s %s %s %s\n%s %s %s %s %s %s %s\n%s%s%s%s%s%s%s\n%s %s %s %s %s %s %s\n"
				current
				cur-cond
				cur-temp
				day-1
				day-2
				day-3
				day-4
				day-5
				day-6
				day-7
				date-1
				date-2
				date-3
				date-4
				date-5
				date-6
				date-7
				cond-1
				cond-2
				cond-3
				cond-4
				cond-5
				cond-6
				cond-7
				temp-1
				temp-2
				temp-3
				temp-4
				temp-5
				temp-6
				temp-7))
  (message "%s" weather-string))

(provide 'weather)
