;;; search-files.el --- Search all files in current directory (not recursive) for a string
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 21 Feb 2014

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

;; Search all files in current directory (not recursive) for a string.

(defun search-files ()
  (interactive)
  (setq searchstr (read-from-minibuffer "Search files for: "))
  (setq dirlist (directory-files-and-attributes "."))
  (setq curbuff (current-buffer))
  (setq resultsbuff (generate-new-buffer "*Search Results*"))
  (while dirlist
	;; Each file
	(let* ((file (car dirlist))
		   (filename (car file))
		   (attributes (nth 9 file)))
	  ;; Only files, no directories
	  (when (not (string= (substring attributes 0 1) "d"))
		(setq tempbuff (generate-new-buffer "*temp*"))
		(switch-to-buffer tempbuff)
		(insert-file-contents filename)
		;; Find first result
		(setq found (search-forward searchstr nil t))
		(when found
		  (switch-to-buffer resultsbuff)
		  ;; Show the filename containing results
		  (insert (format"%s:\n" filename))
		  (switch-to-buffer tempbuff)
		  (while found
			;; Kill result
			(move-beginning-of-line nil)
			(kill-line)
			(setq linum (line-number-at-pos))
			(switch-to-buffer resultsbuff)
			;; Yank result with line number
			(insert (format "%d: " linum))
			(insert (format "%s\n" (car kill-ring-yank-pointer)))
			(switch-to-buffer tempbuff)
			;; Find next result
			(setq found (search-forward searchstr nil t)))
		  (switch-to-buffer resultsbuff)
		  (insert "\n"))
		(kill-buffer tempbuff)))
	(setq dirlist (cdr dirlist)))
  (switch-to-buffer curbuff) ; Restore buffer
  (pop-to-buffer resultsbuff) ; Show the results in a different window
  (goto-line 0)) ; Go to the start of the results

;; (global-set-key (kbd "C-c s") 'search-files)

(provide 'search-files)
