;;; jump-to-window-configuration.el --- Jump to a saved window configuration in register
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 12 Mar 2014

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

;; Jump to a saved window configuration in register

(defun jump-to-window-configuration (n)
  (interactive "p")
  (if (window-configuration-p (car (get-register n)))
	  (jump-to-register n)
	(error "Register doesn't contain a window configuration")))

;; (global-set-key (kbd "C-c 0") (lambda () (interactive) (jump-to-window-configuration ?0)))
;; (global-set-key (kbd "C-c 1") (lambda () (interactive) (jump-to-window-configuration ?1)))
;; (global-set-key (kbd "C-c 2") (lambda () (interactive) (jump-to-window-configuration ?2)))
;; (global-set-key (kbd "C-c 3") (lambda () (interactive) (jump-to-window-configuration ?3)))
;; (global-set-key (kbd "C-c 4") (lambda () (interactive) (jump-to-window-configuration ?4)))
;; (global-set-key (kbd "C-c 5") (lambda () (interactive) (jump-to-window-configuration ?5)))
;; (global-set-key (kbd "C-c 6") (lambda () (interactive) (jump-to-window-configuration ?6)))
;; (global-set-key (kbd "C-c 7") (lambda () (interactive) (jump-to-window-configuration ?7)))
;; (global-set-key (kbd "C-c 8") (lambda () (interactive) (jump-to-window-configuration ?8)))
;; (global-set-key (kbd "C-c 9") (lambda () (interactive) (jump-to-window-configuration ?9)))

(provide 'jump-to-window-configuration)
