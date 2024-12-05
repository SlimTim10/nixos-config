;; Packages
(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(electric-pair-mode t)
 '(image-dired-thumb-height 250)
 '(image-dired-thumb-size 250)
 '(image-dired-thumb-width 250)
 '(indent-tabs-mode nil)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(auto-dim-other-buffers drag-stuff elm-mode yaml-mode web-mode use-package tide smex rust-mode ruby-end rjsx-mode php-mode nix-mode markdown-preview-mode magit haskell-mode gptel go-mode go flx emmet-mode elixir-mode dumb-jump counsel-projectile avy ag))
 '(subword-mode 1 t)
 '(typescript-indent-level 2))
(package-initialize)

;; Startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(global-hl-line-mode 1)
(setq hl-line-sticky-flag nil)
(delete-selection-mode 1)
(setq confirm-kill-emacs #'y-or-n-p)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq lock-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq delete-by-moving-to-trash t)
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  (delete-dups load-path))
(global-eldoc-mode -1)
(setq bookmark-save-flag 1)
(setq tab-bar-mode t)
(setq tab-bar-show t)
(setq package-install-upgrade-built-in t)

;; Better garbage collection
(require 'gcmh)
(gcmh-mode 1)

;; Auto revert files when they change
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Use file system notifications instead of polling
(setq auto-revert-use-notify t)
(setq auto-revert-avoid-polling t)
;; OR:
;; Use polling instead of file system notifications
;; (setq auto-revert-use-notify nil)
;; (setq auto-revert-avoid-polling nil)
;; (setq auto-revert-interval 1) ; check for changes every second

;; Title
(setq display-time-format "%l:%M %p  %a, %b %e, %Y")
(setq display-time-default-load-average nil)
(display-time-mode 1)
(setq frame-title-format
	  '(multiple-frames "Emacs - %b" ("" invocation-name "@" system-name " |" display-time-string)))

;; Mode line
(column-number-mode 1)
(set-face-attribute 'mode-line nil :height 80)
(set-face-attribute 'mode-line-inactive nil :height 80)
(setq
 mode-line-position
 '(" | "
   (line-number-mode ("%l" (column-number-mode ":%c")))
   (" %p")))
;; Remove minor modes
(setq
 mode-line-modes
 (mapcar
  (lambda (elem)
	(pcase elem
	  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
	   "")
	  (_ elem)))
  mode-line-modes))
(defadvice vc-mode-line (after my-after-vc-mode-line () activate)
  (when (stringp vc-mode)
	(setq vc-mode (concat " |" vc-mode)))) ; pipe bracket for git info in mode line
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   ;; mode-line-mule-info
   ;; mode-line-client
   mode-line-modified
   ;; mode-line-remote
   ;; mode-line-frame-identification
   mode-line-buffer-identification
   mode-line-position
   (vc-mode vc-mode)
   " | "
   mode-line-modes
   ;; mode-line-misc-info
   mode-line-end-spaces))

;; Handle word wrapping
(global-visual-line-mode 1)

;; General programming
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-indent-mode 1)
(electric-pair-mode 1)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Desktop mode
(use-package desktop
  :init
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (defun my-desktop-save ()
	(interactive)
	;; Don't call desktop-save-in-desktop-dir, as it prints a message.
	(if (eq (desktop-owner) (emacs-pid))
		(desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save)
  :config
  (desktop-save-mode 1) ; Auto-save
  )

;; Frames only mode
(use-package frames-only-mode
  :config
  (frames-only-mode 1))

;; eyebrowse
;; (eyebrowse-mode t)

;; winner
(use-package winner
  :load-path "~/.emacs.d/lisp"
  :config
  (winner-mode 1)
  (global-set-key (kbd "C-c C-/") 'winner-undo)
  )

;; Company mode
(use-package company
  :config
  (global-company-mode -1))

;; C programming
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t
			  c-tab-always-indent nil)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode)) ; Arduino
(add-hook 'c-mode-hook
		  (lambda ()
			(subword-mode 1)
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)))

;; Haskell programming
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook
		  (lambda ()
			(subword-mode 1)
			(local-unset-key (kbd "M-.")) ; xref-find-definitions for dumb-jump
			(local-unset-key (kbd "C-c C-f"))
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-unset-key (kbd "C-c C-b"))
			(local-set-key (kbd "C-c C-b") 'backward-sexp)
			(local-unset-key (kbd "<C-backspace>"))
			(local-set-key (kbd "<C-backspace>") 'backward-kill-sexp)
			(local-set-key (kbd "C-c C-.") 'haskell-mode-jump-to-def))))
;; (package-install 'intero)

;; JavaScript/React programming
(use-package js2-mode
  :init
  (setq js2-strict-missing-semi-warning nil))
(use-package rjsx-mode)

;; Ruby programming
(use-package ruby-end)

;; JavaScript programming
(add-hook 'js-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			;; (setq indent-tabs-mode t)
			(setq js-indent-level 2)
			;; (setq js-indent-level 4)
			(subword-mode 1)
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)
			))

;; TypeScript programming
(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))
(use-package apheleia
  :ensure t
  :defer t
  :hook ((typescript-ts-mode . apheleia-mode)
         (tsx-ts-mode . apheleia-mode)
         (js-ts-mode . apheleia-mode)
         (go-ts-mode . apheleia-mode)))

;; Python programming
(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(setq tab-width 4)
			(local-unset-key (kbd "C-c C-f"))
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)))
(setq gud-pdb-command-name "python -m pdb")

;; PHP programming
(use-package php-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  (add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)
  (add-hook 'php-mode-hook
			(lambda ()
			  (set (make-local-variable 'company-backends)
				   '((php-extras-company company-dabbrev-code) company-capf company-files))
			  (company-mode 1)
			  (setq company-idle-delay 0)))
  )

;; Web programming
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (defun my-web-mode-hook ()
	"Settings for Web mode."
	(setq tab-width 2)
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
	(emmet-mode))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  )

;; Emmet
(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ; Auto-start on any markup modes
  (add-hook 'css-mode-hook 'emmet-mode) ; Enable Emmet's css abbreviation
  (add-hook 'sgml-mode-hook
			(lambda ()
			  (local-set-key (kbd "C-c C-f") 'sgml-skip-tag-forward)
			  (local-set-key (kbd "C-c C-b") 'sgml-skip-tag-backward)))
  (setq emmet-move-cursor-between-quotes t)
  )

;; CSS
(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (setq indent-tabs-mode nil)
  )

;; HTML mode
(add-hook 'html-mode-hook
		  (lambda ()
			(local-unset-key (kbd "C-c 0"))
			(local-unset-key (kbd "C-c 1"))
			(local-unset-key (kbd "C-c 2"))
			(local-unset-key (kbd "C-c 3"))
			(local-unset-key (kbd "C-c 4"))
			(local-unset-key (kbd "C-c 5"))
			(local-unset-key (kbd "C-c 6"))
			(local-unset-key (kbd "C-c 7"))
			(local-unset-key (kbd "C-c 8"))
			(local-unset-key (kbd "C-c 9"))))

;; Rust programming
(use-package rust-mode)

;; Quick way to reload .emacs configuration
(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

;; Personal packages
(require 'search-files)
(global-set-key (kbd "C-c s") 'search-files)
(require 'jump-to-window-configuration)
(global-set-key (kbd "C-c 0") (lambda () (interactive) (jump-to-window-configuration ?0)))
(global-set-key (kbd "C-c 1") (lambda () (interactive) (jump-to-window-configuration ?1)))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (jump-to-window-configuration ?2)))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (jump-to-window-configuration ?3)))
(global-set-key (kbd "C-c 4") (lambda () (interactive) (jump-to-window-configuration ?4)))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (jump-to-window-configuration ?5)))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (jump-to-window-configuration ?6)))
(global-set-key (kbd "C-c 7") (lambda () (interactive) (jump-to-window-configuration ?7)))
(global-set-key (kbd "C-c 8") (lambda () (interactive) (jump-to-window-configuration ?8)))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (jump-to-window-configuration ?9)))
(require 'google)
(global-set-key (kbd "C-c g") 'google)
(require 'nzbsearch)
(require 'stack-overflow)
(require 'weather)
(require 'tea-timer)
(require 'imdb)
(require 'mail-me)
(require 'my-mail-to)
(require 'calculate-money-earned)
(require 'xah)
(require 'win-audio)

;; Move forward in mark ring
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

;; Easier window resizing
(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

;; Set fonts
(set-frame-font "Fira Code 14" nil t)
(set-face-attribute 'default nil :font "Fira Code-14")
(use-package ligature
  :config
  ;; Enable all Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Dired
(require 'ls-lisp)
(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode))
  :bind
  (:map
   dired-mode-map
   ("C-c C-p" . dired-prev-subdir)
   ("C-c C-n" . dired-next-subdir)
   ("k" . dired-kill-and-next-subdir) ; Opposite of "i"
   ("RET" . xah-open-in-external-app)
   ("DEL" . dired-up-alternate-directory)
   ("<tab>" . dired-subtree-toggle)
   )
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-listing-switches "-alhv")
  (setq dired-dwim-target t) ; Try to guess a default target directory
  (setq dired-recursive-copies 'always) ; "always" means no asking
  (setq dired-recursive-deletes 'always) ; Delete recursively without asking
  (setq dired-isearch-filenames t) ; Limit search commands to file names
  (put 'dired-find-alternate-file 'disabled nil) ; Enable useful command
  )
(use-package dired-subtree :ensure t :after dired)

;; dired-rsync
(use-package dired-rsync
  :bind
  (:map
   dired-mode-map
   ("C-c C-r" . dired-rsync)))
;; dired-rsync-transient wraps the command in a magit-like transient interface allowing you to tweaks the parameters for your call.
(use-package dired-rsync-transient
  :bind
  (:map
   dired-mode-map
   ("C-c C-x" . dired-rsync-transient)))

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that.
URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(defun dired-kill-and-next-subdir ()
  (interactive)
  (let* ((subdir-name (dired-current-directory))
         (parent-dir  (file-name-directory (directory-file-name subdir-name)))
         (search-term (concat " " (file-name-nondirectory (directory-file-name subdir-name)))))
    (dired-kill-subdir)
    (dired-goto-subdir parent-dir)
    (search-forward search-term)))

(defun dired-up-alternate-directory ()
  (interactive)
  (find-alternate-file ".."))

;; Colour theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'dracula t)
(set-mouse-color "green")

;; Ivy, Counsel, Swiper
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  ;; ; Use flx for fuzzy matching
  ;; (setq ivy-re-builders-alist
  ;; 	  '((_ . ivy--regex-fuzzy))) ; Default matching where space is .*
  (setq ivy-re-builders-alist
		'((t . ivy--regex-plus))) ; No initial ^ character
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "C-c C-j") 'ivy-immediate-done)
  :config
  (ivy-mode 1)
  :bind
  ("M-l" . switch-to-buffer)
  )
(use-package counsel
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  )
(use-package swiper
  :init
  (global-set-key (kbd "C-s") 'swiper-isearch)
  )
(use-package colir
  :ensure nil
  )
(use-package smex)
(use-package flx)

;; magit
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  )

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(defun my-rename-uniquely ()
  (interactive)
  (let ((current-folder (car (last (split-string default-directory "/") 2))))
	(rename-buffer (concat (buffer-name) "<" current-folder ">"))))

;; SMTP
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)

;; Org mode
(let ((default-directory "~/Sync/Notes"))
  (setq org-directory default-directory)
  (setq my-org-path (list default-directory))
  (setq org-default-notes-file (expand-file-name "notes.org")))
(setq org-log-done "time") ; Display timestamp for finished TODO items
(setq org-src-fontify-natively t)
(setq org-adapt-indentation nil)
(setq org-src-preserve-indentation t)
(setq org-id-link-to-org-use-id t) ; Use IDs for header links (create if not exist)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
	  '(("t" "Task" entry (id "MISC-TASKS-EVENTS")
		 "* TODO %?\n" :jump-to-captured t :empty-lines 1)
		("e" "Event" entry (id "MISC-TASKS-EVENTS")
		 "* %?\n%^T\n" :jump-to-captured t :empty-lines 1)
		("n" "Note" entry (id "MISC-NOTES")
		 "* %?\n %U\n" :jump-to-captured t :empty-lines 1)
        ("s" "Session" entry (file buffer-file-name)
         "
* Session

%^t

** Plan

- ?

** Summary

- ?

** Goals

- ?

** My tasks

- [ ] Send session recording
  - ?
- [ ] Payment ($?)
  - [ ] Send invoice (#0000)
  - [ ] Paid
"
         :jump-to-captured t
         :empty-lines 1)
        ("c" "New contact" entry (id "CONTACTS")
         "
* %?
:PROPERTIES:
:Address: ?
:Phone: ?
:Email: ?
:LinkedIn: ?
:END:
"
         :jump-to-captured t
         :empty-lines 1)
        ))
(setq
 org-file-apps
 '((auto-mode . emacs)
   (directory . emacs)
   ("pdf" . default)
   ("jpg" . default)
   ("svg" . default)
   ("txt" . emacs)
   (t . "xdg-open %s")))
(setq org-goto-interface 'outline-path-completion)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil) ; work nicely with ivy
(setq org-export-html-postamble nil)
(setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
(setq org-cycle-separator-lines 0)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-log-into-drawer t)
(setq org-todo-keywords
 '((sequence "TODO" "DOING" "|" "DONE"))
 )
(setq org-todo-keyword-faces
 '(("DOING" . "magenta"))
 )

;; Run this to archive all "DONE" tasks in a file
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

;; Agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files my-org-path)
(setq org-sort-agenda-notime-is-late nil) ; items without time are put at the top of the day
(setq org-refile-targets
	  (quote ((nil :maxlevel . 6)
			  (org-agenda-files :maxlevel . 6))))
(setq org-agenda-timegrid-use-ampm t)
;; breadcrumbs
;; (setq org-agenda-prefix-format
;; 	  '((agenda . " %i %-12:c%?-12t% s %b")
;; 		(timeline . "  % s")
;; 		(todo . " %i %-12:c")
;; 		(tags . " %i %-12:c")
;; 		(search . " %i %-12:c")))
;; (setq org-agenda-breadcrumbs-separator "/")

;; avy
(use-package avy
  :init
  (setq avy-keys '(?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avy-style 'at-full)
  (setq avy-all-windows 'all-frames)
  :config
  (add-to-list 'avy-orders-alist '(avy-goto-word-1 . avy-order-closest))
  (global-set-key (kbd "C-.") 'avy-goto-char-timer)
  )

;; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)))

;; Scheme mode
(add-hook 'scheme-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)
			(local-set-key (kbd "C-c C-p") 'backward-up-list)
			(local-set-key (kbd "C-c C-n") 'down-list)))

;; Projectile
(use-package counsel-projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-global-mode)
  :bind
  (:map
   projectile-mode-map
   ("C-x p s" . projectile-ripgrep))
  )

;; Eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
	(erase-buffer)
	(eshell-send-input)))
(add-hook 'eshell-mode-hook
		  (lambda ()					; Add directory path to buffer name
			(let ((bufname (concat (buffer-name) default-directory)))
			  (message bufname)
			  (if (get-buffer bufname)
				  (progn
					(kill-buffer)
					(switch-to-buffer bufname))
				(rename-buffer bufname)))))
;; (print eshell-mode-hook)
;; (remove-hook 'eshell-mode-hook (first eshell-mode-hook))

;; GDB
(setq gdb-many-windows t)
(setq gud-gdb-command-name "arm-none-eabi-gdb -i=mi")

;; Expand
(require 'setup-hippie)
(setq hippie-expand-try-functions-list
	  '(try-expand-dabbrev-closest-first
		try-expand-line-closest-first
		try-expand-dabbrev-from-kill
		try-complete-file-name-partially
		try-complete-file-name
		try-expand-dabbrev-all-buffers
		try-expand-all-abbrevs
		try-expand-list
		try-complete-lisp-symbol-partially
		try-complete-lisp-symbol))

;; YAML mode
(add-hook 'yaml-mode-hook
		  (lambda ()
			(define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; dumb-jump
(use-package dumb-jump
  :init
  (setq dumb-jump-force-searcher 'ag)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

;; nix
(use-package nix-mode)

(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

(defun run-javascript-file ()
  "Runs entire javascript file in eshell buffer (creates a new one if it does not exist)."
  (interactive)
  (let* ((file (buffer-file-name))
		 (buf (get-buffer-create (concat "*eshell*" (expand-file-name default-directory)))))
	(with-current-buffer buf
	  (print buf)
	  (eshell/clear)
	  (eshell-return-to-prompt)
	  (insert (concat "node " "\"" file "\""))
	  (eshell-send-input))))

;; Tramp on Windows
;; plink.exe (from PuTTY) needs to be in PATH
(when (eq window-system 'w32)
  (setq tramp-default-method "plink"))

;; ledger
(use-package ledger-mode
  :mode ("\\.journal\\'")
  :config
  (setq ledger-mode-should-check-version nil)
  (setq ledger-report-links-in-register nil)
  (setq ledger-binary-path "hledger")
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold t)
  (setq ledger-complete-in-steps t)
  )

;; direnv
(use-package envrc
  :ensure t
  :if (executable-find "direnv")
  :demand t
  :commands
  (envrc-allow
   envrc-deny
   envrc-file-mode
   envrc-global-mode
   envrc-propagate-environment
   envrc-reload)
  :mode ("\\.envrc\\'" . envrc-file-mode)
  :config
  (envrc-global-mode))

;; agda
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; View PDFs
;; Installing it with use-package doesn't work. Install it using package-install, then run pdf-tools-install.
;; (use-package pdf-tools
;;   :ensure t
;;   :mode  ("\\.pdf\\'" . pdf-view-mode)
;;   :config
;;   (require 'pdf-tools)
;;   (require 'pdf-view)
;;   (require 'pdf-misc)
;;   (require 'pdf-occur)
;;   (require 'pdf-util)
;;   (require 'pdf-annot)
;;   (require 'pdf-info)
;;   (require 'pdf-isearch)
;;   (require 'pdf-history)
;;   (require 'pdf-links)
;;   (pdf-tools-install :no-query)
;;   (setq-default pdf-view-display-size 'fit-page)
;;   (setq pdf-annot-activate-created-annotations t))

;; ripgrep
(use-package rg
  :ensure-system-package rg)

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

;; Visual undo
(use-package vundo
  :bind (("C-x u" . vundo)
         ("C-/" . undo-only)
         ("C-?" . undo-redo))
  :config
  (setq vundo-glyph-alist vundo-ascii-symbols))


;; My custom keys

(defvar my/keys-keymap (make-keymap)
  "Keymap for my/keys-mode")

(define-minor-mode my/keys-mode
  "Minor mode for my personal keybindings."
  :init-value t
  :global t
  :keymap my/keys-keymap)

;; Don't let arbitrary modes take precedence over my global keybindings.
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((my/keys-mode . ,my/keys-keymap)))

;; My custom bindings
(define-key my/keys-keymap (kbd "M-o") (lambda () (interactive) (other-window 1)))
(define-key my/keys-keymap (kbd "M-O") (lambda () (interactive) (other-window -1)))
(define-key my/keys-keymap (kbd "M-/") 'hippie-expand)
(define-key my/keys-keymap (kbd "C-<") 'pop-to-mark-command)
(define-key my/keys-keymap (kbd "C->") 'unpop-to-mark-command)
(define-key my/keys-keymap (kbd "M-k") 'kill-current-buffer)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "dim gray"))))
 '(mode-line-inactive ((t (:background nil :foreground "gray")))))
