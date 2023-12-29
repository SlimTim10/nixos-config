;;; goto-last-change.el --- Move point to (and mark) most recent change(s)
;; Copyright (C) 2013  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 17 Feb 2013

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

;; Move point to last change made. If the change is an insertion, mark the entire change, otherwise do not mark.
;; Provide numeric prefix argument to choose change, starting with 1 being the most recent.
;; E.g., "C-3 M-x goto-last-change" will go to the 3rd last change.

(defun goto-last-change (&optional num-change &optional change-list)
  (interactive "p")
  (when (eq buffer-undo-list nil)
    (error "No change information"))
  (when (eq change-list nil)
    (setq change-list buffer-undo-list))
  (when (eq num-change nil)
    (setq num-change 0))
  (setq num-change (abs num-change))
  (cond ((<= num-change 1)		; Base case
	 (when (not (eq (car change-list) nil))
	   (error "No further change information"))
	 (if (stringp (car-safe (cadr change-list))) ; Last change was deletion
	     (goto-char (abs (cdr-safe (cadr change-list)))) ; Go to the position of the deletion (last change)
	   (progn
	     (goto-char (abs (car-safe (cadr change-list)))) ; Go to the start of the last change
	     (set-mark-command nil)			     ; Activate the mark
	     (goto-char (abs (cdr-safe (cadr change-list))))))) ; Go to the end of the last change
	((eq (car change-list) nil)				; First element being nil means good element
	 (goto-last-change (- num-change 1) (cddr change-list))) ; Count down
	((eq (cdr change-list) nil)				     ; End of the list
	 (error "No further change information"))
	(t (goto-last-change num-change (cdr change-list))))) ; Don't count bad elements

;; Move point to last change made. No marking.

(defun goto-last-change-no-mark ()
  (interactive)
  (if (stringp (car-safe (cadr buffer-undo-list))) ; Last change was deletion
      (goto-char (abs (cdr-safe (cadr buffer-undo-list)))) ; Go to the position of the deletion (last change)
      (goto-char (abs (cdr-safe (cadr buffer-undo-list)))))) ; Go to the end of the last change

;; (global-set-key (kbd "C-x C-.") 'goto-last-change)

(provide 'goto-last-change)
(provide 'goto-last-change-no-mark)
