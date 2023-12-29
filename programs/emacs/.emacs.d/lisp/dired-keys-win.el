;;; dired-keys-win.el --- Easier way to open files (for Windows, using 'file' command from Cygwin)
;; Copyright (C) 2015  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 11 Feb 2015

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

;; Operate on a file depending on its type as determined by 'file' command
(defun my-dired-operate-on-file ()
  "Operate on a file depending on its type."
  (interactive)
  (let* ((file-name (dired-get-file-for-visit))
		 (file-type (shell-command-to-string (concat "file " (shell-quote-argument file-name)))))
	(message file-name)
	;; Open text files, empty files, and directories in emacs, all other files in their default associated program
	(if (or (string-match "text" file-type)
			(string-match "empty" file-type)
			(string-match "directory" file-type))
		(dired-find-alternate-file)
	  (w32-shell-execute "open" file-name))))

(defun dired-w32-open-file ()
  "Open a file in Windows with default program."
  (interactive)
  (w32-shell-execute "open" (dired-get-filename nil t)))
