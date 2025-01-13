;; Change Default Directory
(setq default-directory "~/.emacs.d/user-files/")
;; /// GLOBAL SET KEYS ///
;; General
(global-set-key (kbd "<f9>")    'compile)
(global-set-key (kbd "C-c e")   (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c g")   'replace-regexp)
(global-set-key (kbd "C-c i")   'insert-char)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c p")   'package-list-packages)
(global-set-key (kbd "C-c v")   'view-mode)
(global-set-key (kbd "C-q")     'kill-current-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "M-o")     'ace-window)
(global-set-key [(control tab)] 'hippie-expand)
;; ORG-MODE
(global-set-key (kbd "<f7>")  'org-toggle-inline-images)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c h") 'outline-hide-subtree)
(global-set-key (kbd "C-c o") 'org-open-at-point)
(global-set-key (kbd "C-c s") 'org-toggle-heading)
(global-set-key (kbd "C-c t") 'org-todo-list)
;; Work with Buffers
(global-set-key (kbd "<f12>")   'save-buffers-kill-emacs)
(global-set-key (kbd "<f4>")    'eval-buffer)
(global-set-key (kbd "<f5>")    'save-buffer)
;; Work with Clipboard
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(delete-selection-mode t)
;; Sorting
(global-set-key (kbd "C-c C-p") 'sort-paragraphs)
(global-set-key (kbd "C-c C-l") 'sort-lines)
;; Recent Files
(global-set-key (kbd "C-c C-r") 'recentf-open-files)
;; FONT
(global-set-key (kbd "C-c -") 'text-scale-decrease)
(global-set-key (kbd "C-c =") 'text-scale-increase)
(set-frame-font "Source Code Pro Italic 14")
;;M-x cfw:open-org-calendar
(global-set-key (kbd "C-c C-c") 'cfw:open-org-calendar)
;; Work with Region
(global-set-key (kbd "<f8>")  'comment-region)
(global-set-key (kbd "C-x w") 'write-region)
;; Google Translate
(global-set-key (kbd  "\C-c t") 'google-translate-at-point)
(global-set-key (kbd "\C-c T")  'google-translate-query-translate)
(global-set-key (kbd "\C-c r")  'google-translate-query-translate-reverse)
;;(global-set-key (kbd "C-c l")   (lambda () (interactive)(find-file "~/ORGS/Lib.org")))

;; FUNCTIONS
;; Auto kill completion
(add-hook 'minibuffer-exit-hook
	  #'(lambda () (let ((buffer "*Completions*"))
			(and (get-buffer buffer)
			     (kill-buffer buffer)))))
;; IBuffer
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
    (quote 	
		(
	("default"
	       ("Org" (mode . org-mode))
	       ("Programming C/C++" (or (mode . c-mode) (mode . c++-mode)))
	       ("Python" (mode . python-mode))
	       ("Emacs-Lisp" (or (mode . emacs-lisp)(name . ".el")))
	       ("Text" (name . ".txt"))
	)
		)
	)
)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; Ivy
(use-package ivy
  :ensure t
  :init
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Counsel
(use-package counsel
 :bind
 ([remap insert-char] . counsel-unicode-char))

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(defun dired-show-dotfiles ()
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p))
	    dired-dotfiles-show-p)
	(progn
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer)
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))

;;--------------------------------------------------GUI
(tool-bar-mode     					-1)
(tooltip-mode      					-1)
(scroll-bar-mode   					-1)
(menu-bar-mode     					-1)
(blink-cursor-mode 					-1)
(global-display-line-numbers-mode 	                 t)
(setq use-dialog-box 				       nil)
(defalias 'yes-or-no-p 				 'y-or-n-p)
(setq inhibit-startup-message 		                 t)
(setq inhibit-startup-screen                             t)
(setq inhibit-splash-screen 		                 t)

;; bind useful commands to C-h C-l and C-h C-i
(use-package help
  :bind
  ("C-h C-l" . find-library)
  ("C-h C-i" . info-display-manual))

;; Highlight my words, baby
(auto-fill-mode						t)
(column-number-mode                                     t)
(electric-pair-mode					t)
(show-paren-mode         				t)
(global-hl-line-mode     				t)
(global-visual-line-mode 				t)
(setq initial-scratch-message			       "")
(setq make-backup-files					nil)
(setq message-log-max					t)
(setq scroll-preserve-screen-position	                1)
(setq scroll-step					1)
(setq show-paren-style 					'expression)
(setq truncate-lines					t)
;; For notebooks only
;;(display-battery-mode                                 t)
(size-indication-mode                                   t)

;; TIME
(use-package time
  :init
  (display-time-mode             t)
  (setq display-time-24hr-format t)
)

;; Package Manager - Paradox
(use-package paradox
  :ensure t
  :init
   (paradox-enable)
 )

(require 'package)
(setq package-archives '(	("gnu"   . "http://elpa.gnu.org/packages/")
				("melpa" . "https://melpa.org/packages/")
				("org"   . "http://orgmode.org/elpa/")
			)
)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; Python
(setq python-indent-offset 4)

(use-package TeX
:defer t
:config
(
  (setq TeX-auto-save   t)
  (setq TeX-parse-self   t)
  (setq-default TeX-master)
)
)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-setup ()
  (auto-complete-mode t)
  (setq ac-sources (append '(
			     ac-source-math-unicode
			     ac-source-math-latex
			     ac-source-latex-commands
			     )
			   ac-sources
			   )
	)
  )
(add-hook 'LaTeX-mode-hook 'ac-latex-setup)
(setq ac-math-unicode-in-math-p t)

;; Recent Files
(use-package recentf
  :init
  (setq recentf-max-saved-items 10)
  :config
  (recentf-mode t)
  (recentf-cleanup)  
)
;;thththththththththththththththththth Themes
;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold nil)
  (setq doom-themes-enable-italic nil)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
)

;; ACE WINDOW
(require 'ace-window)
(setq ace-window-display-mode t)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)
(defvar aw-dispatch-alist
  '(
        (?? aw-show-dispatch-help)
	(?M aw-move-window                "Move Window")
	(?b aw-split-window-horz          "Split Horz Window")
	(?c aw-copy-window                "Copy Window")
	(?c aw-split-window-fair          "Split Fair Window")
	(?j aw-switch-buffer-in-window    "Select Buffer")
	(?m aw-swap-window                "Swap Windows")
	(?n aw-flip-window                "Flip Windows")
	(?o delete-other-windows          "Delete Other Windows")
	(?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	(?v aw-split-window-vert          "Split Vert Window")
	(?x aw-delete-window              "Delete Window")
   )
  "List of actions for `aw-dispatch-default'.")

(use-package diminish
  :config
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode)
  (diminish 'ivy-mode)
  (diminish 'auto-fill-mode)
  (diminish 'flycheck-mode))

;; ELFEED - RSS NEWS READER
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
'(
  ;; CNN 
  ("rss.cnn.com/rss/edition.rss" eng news)
  ("rss.cnn.com/rss/edition_world.rss" eng news)  
  ("rss.cnn.com/rss/edition_entertainment.rss" eng news)
  ;; IT
  ("infoworld.com/index.rss" eng it)
  ("www.computerweekly.com/rss/All-Computer-Weekly-content.xml" eng it)
  ("www.scnsoft.com/blog/rss" eng it cybersec)
  ("https://blog.paessler.com/rss.xml" eng it network)
  ("www.techcracked.com/feeds/posts/default?alt=rss" eng it courses)
  ("infotechkeeda.com/feed/" eng it news)
  ;; RU CHANNELS 
  ("24smi.org/rss/" ru) ;; NEWS
  ("pcnews.ru/feeds/special/news.rss" ru pc) ; PC NEWS
  ;; ("www.igromania.ru/rss/rss_news.xml" ru games) ;; Igromania - NO SUPPORTED ELFEED
  ;; NULL PROGRAM
  ("nullprogram.com/feed/" prog)
  ;; Reddit
  ("www.reddit.com/r/emacs/.rss" reddit emacs)
  ("www.reddit.com/r/news/.rss" reddit news) 
  ))
  (setq-default elfeed-search-filter "@1-month-ago +unread"))

;; Display feed URL
(defun my-display-current-entry-feed ()
  (interactive)
  (let ((entry (elfeed-search-selected t)))
    (when entry
      (let ((feed (elfeed-entry-feed entry)))
        (message "%s" (elfeed-feed-url feed))))))

;; Change to correct date and time
(defun elfeed-search-format-date (date)
  (format-time-string "%d-%m-%Y %H:%M" (seconds-to-time date)))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
)

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-backend-method 'curl)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ru")
  (setq google-translate-pop-up-buffer-set-focus t)
)

 (defun google-translate--search-tkk ()
   "Search TKK."
   (list 430675 2721866130))

;; HIhiHIhiHIhiHIhiHIhiHIhiHIhiHIhiHI Hippie Expand
(setq hippie-expand-try-functions-list
	'(
	  try-expand-dabbrev-visible
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	)
)

;; Multicursors
(require 'multiple-cursors)

;; Russian keybindings equal English keybindings
(use-package reverse-im
  :ensure t
  :config
  (reverse-im-activate "russian-computer")
)
(require 'use-package)
(which-key-mode t)

;; Babel
(setq org-babel-load-languages
      '(
	(emacs-lisp . t)
	(shell .      t)
	(python .     t)
	(C .          t)
	(latex .      t)
	)
      )

;; Work with Org-mode
(setq org-log-done 'time)
(setq org-support-shift-select t)
(setq org-confirm-babel-evaluate nil)
(setq org-use-property-inheritance (quote ("DEADLINE")))
;; European Date Format
(setq org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %H:%M>"))
(setq org-display-custom-times t)
(add-hook 'calendar-mode-hook (lambda () (calendar-set-date-style 'european)))
(setq calendar-week-start-day 1)
(require 'calfw)
(require 'calfw-org)
;; use org agenda buffer style keybinding.
(setq cfw:org-overwrite-default-keybinding t)

;; Exporting
(require 'ox-md)

;; Always folded files
(setq org-startup-folded t)

;; Insert code block quickly
(defun my/org-insert-src-block(src-code-type)
  "Insert a 'src-code-type' type source code block in org-mode"
  (interactive
   (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "C++" "css" "html" "js" "lisp" "latex" "org")))
     (list (ivy-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")))

;; Rename links
(defun my/org-rename-link ()
  "Rename the file pointed by the link under point and update the link."
  (interactive)
  (when (org-in-regexp org-link-bracket-re 1)
    (let* ((remove (list (match-beginning 0) (match-end 0)))
           (link (org-link-unescape (match-string-no-properties 1))) 
           (desc (when (match-end 2) (match-string-no-properties 2)))
           (type (progn (string-match
			 "\\`\\([^:]*:\\)?\\([^:]*\\)\\(::.*\\)?" link) (match-string 1 link)))
           (filepath (match-string 2 link))
           (search (match-string 3 link))
           (filename (file-name-nondirectory filepath))
           (path (file-name-directory filepath))
           (newfilename (read-string (format "New name (was '%s'): " filename))))
      ;; Rename file 
      (rename-file (concat path filename) (concat path newfilename))
      ;; Update link
      (unless (string-empty-p newfilename)
        (apply #'delete-region remove)
        (insert (org-link-make-string (concat type path newfilename search) desc))))))

(setq org-treat-insert-todo-heading-as-state-change t)
(setq org-log-into-drawer t)

(defun my/org-summary-todo (n-done n-not-done)
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))

(setq org-agenda-custom-commands
      '(
	("c" "Simple Agenda view" agenda "")
	("d" todo "DONE")
	("t" todo "TODO")
	("s" todo "SCHEDULED")
	)
      )

(setq org-agenda-prefix-format
      '(
	(agenda . " %i %-12:c%?-12t% s")
	(todo   . " %i %-12:c")
	(tags   . " %i %-12:c")
	(search . " %i %-12:c")
	)
      )

;; Rebuild agenda every 10 minutes
(defun rebuild-agenda ()
  (interactive)
  (dolist (buffer (buffer-list))
  (with-current-buffer buffer (when (derived-mode-p 'org-agenda-mode) (org-agenda-maybe-redo))))
  (message (concat "Agenda was updated: "(format-time-string "%H:%M"))))
(run-with-idle-timer 600 t 'rebuild-agenda)

;; Hooks
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; Load AGENDA after start emacs
(add-hook 'after-init-hook #'org-agenda-list)

;; AGENDA
(setq org-agenda-files '("~/AGENDA/"))
(setq org-agenda-show-all-dates         t)
(setq calendar-week-start-day           1)
(setq org-catch-invisible-edits       nil)
(setq org-agenda-skip-deadline-if-done  t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days        14)
(setq org-log-done                      t)
;; Work On Linux
;;(setq org-agenda-files '("/home/ilya/AGENDA/Habits.org" "/home/ilya/AGENDA/Univer.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "15466a777080bcd4f71fea193fd7e4988552919c0e8a09621883aa19166b5099" default))
 '(package-selected-packages
   '(achievements doom-themes counsel theme-looper which-key use-package reverse-im paradox org-bullets multiple-cursors google-translate flycheck elfeed diminish calfw-org calfw ace-window ac-math)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
