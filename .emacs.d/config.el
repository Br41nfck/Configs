(global-set-key (kbd "C-c i") 'insert-char)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c l") (lambda () (interactive)(find-file "~/ORGS/Lib.org")))
(global-set-key (kbd "C-c e") (lambda () (interactive)(find-file "~/.emacs")))
(global-set-key (kbd "C-c g") 'replace-regexp)
(global-set-key (kbd "C-c v") 'view-mode)
(global-set-key (kbd "C-q") 'kill-current-buffer)
(global-set-key (kbd "C-c p") 'package-list-packages)
(delete-selection-mode t)

;; Browser - qutebrowser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; Work with Buffers
(global-set-key (kbd "<f12>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f4>") 'eval-buffer)
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key [(control tab)] 'hippie-expand)

;; Auto kill completion
(add-hook 'minibuffer-exit-hook
	  '(lambda () (let ((buffer "*Completions*"))
			(and (get-buffer buffer)
			     (kill-buffer buffer)))))
;; IBuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"

	       ("Org" (mode . org-mode))

	       ("Programming C/C++" (or (mode . c-mode)
					(mode . c++-mode)
					))

	       ("Python" (mode . python-mode))

	       ("Emacs-Lisp" (or (mode . emacs-lisp)
				 (name . ".el")))

	       ("Text" (name . ".txt"))))))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; Ivy
(use-package ivy
:init
(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) "))

;; Counsel
(use-package counsel
 :bind
 ([remap insert-char] . counsel-unicode-char))

;; Work with Clipboard
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)

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

;; FONT
(global-set-key (kbd "C-c +") 'text-scale-increase)
(global-set-key (kbd "C-c -") 'text-scale-decrease)

;;--------------------------------------------------GUI
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-linum-mode t)
(setq use-dialog-box nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; bind useful commands to C-h C-l and C-h C-i
(use-package help
  :bind
  ("C-h C-l" . find-library)
  ("C-h C-i" . info-display-manual))

;; Highlight my words, baby
(global-hl-line-mode t)
(show-paren-mode t)
(setq show-paren-style 'expression)
(toggle-word-wrap t)
(global-visual-line-mode t)
(toggle-truncate-lines t)
(auto-fill-mode t)

(setq initial-scratch-message "")
(electric-pair-mode t)
(setq-default truncate-lines t)
(setq-default make-backup-files nil)
(setq message-log-max t)
(setq scroll-step 1)
(setq scroll-preserve-screen-position 1)

(line-number-mode t)
(column-number-mode t)
(display-battery-mode t)
(size-indication-mode t)
;; TIME
(use-package time
  :init
  (display-time-mode t)
  (setq display-time-24hr-format t))

;; Package Manager - Paradox
(use-package paradox
  :init
  (paradox-enable))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; Python
(setq python-indent-offset 4)

(use-package TeX
:defer t
:config
((setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master)))

(require 'auto-complete)
(require 'auto-complete-config)

(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-setup ()
  (auto-complete-mode t)
  (setq ac-sources (append '(ac-source-math-unicode
			     ac-source-math-latex
			     ac-source-latex-commands) ac-sources)))
(add-hook 'LaTeX-mode-hook 'ac-latex-setup)
(setq ac-math-unicode-in-math-p t)

;; Recent Files
(use-package recentf
  :init
  (setq recentf-max-saved-items 10)
  :config
  (recentf-mode t)
  (recentf-cleanup)
  :bind
  ("C-c C-r" . recentf-open-files))

;; Sorting
(use-package sort
  :bind
( "C-c C-p" . sort-paragraphs)
( "C-c C-l" . sort-lines))

;;thththththththththththththththththth Themes
;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t nil)

;; ACE WINDOW
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq ace-window-display-mode t)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
	(?m aw-swap-window "Swap Windows")
	(?M aw-move-window "Move Window")
	(?c aw-copy-window "Copy Window")
	(?j aw-switch-buffer-in-window "Select Buffer")
	(?n aw-flip-window)
	(?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	(?c aw-split-window-fair "Split Fair Window")
	(?v aw-split-window-vert "Split Vert Window")
	(?b aw-split-window-horz "Split Horz Window")
	(?o delete-other-windows "Delete Other Windows")
	(?? aw-show-dispatch-help))
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
(setq elfeed-feeds
'(
  ;; NEWS
  
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
  ("www.igromania.ru/rss/rss_news.xml" ru games) ;; Igromania

  ;; NULL PROGRAM
  ("nullprogram.com/feed/" prog)

  ;; Reddit
  ("www.reddit.com/r/emacs/.rss" reddit emacs)
  ("www.reddit.com/r/news/.rss" reddit news) 
  ))
(setq-default elfeed-search-filter "@1-month-ago +unread")

(use-package flycheck
  :config
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))

(require 'google-translate)
(setq google-translate-backend-method 'curl)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ru")
(setq google-translate-pop-up-buffer-set-focus t)  

(use-package google-translate
  :bind
  (("\C-c t" . google-translate-at-point)
   ("\C-c T" . google-translate-query-translate)
   ("\C-c r" . google-translate-query-translate-reverse)))

 (defun google-translate--search-tkk ()
   "Search TKK."
   (list 430675 2721866130))

;; HIhiHIhiHIhiHIhiHIhiHIhiHIhiHIhiHI Hippie Expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))

(require 'multiple-cursors)

;; Russian keybindings equal English keybindings
(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(require 'use-package)

(which-key-mode t)

;; Babel
(setq org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (C . t)
     (latex . t)))
(setq org-confirm-babel-evaluate nil)

(use-package org-mode
:bind
(( "<f7>" . org-toggle-inline-images)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c h" . outline-hide-subtree)
   ("C-c o" . org-open-at-point)
   ("C-c s" . org-toggle-heading)
   ("C-c t" . org-todo-list)))

;; Work with Org-mode
(setq org-log-done 'time)
(setq org-support-shift-select t)
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
;;M-x cfw:open-org-calendar
(global-set-key (kbd "C-c C-c") 'cfw:open-org-calendar)

;; Exporting
(require 'ox-md)

;; Always folded files
(setq org-startup-folded t)

;; Insert code block quickly
(defun my/org-insert-src-block(src-code-type)
  "Insert a 'src-code-type' type source code block in org-mode"
  (interactive
   (let ((src-code-types '(
			   "emacs-lisp" "python" "C" "sh" "C++" "css" "html" "js" "lisp" "latex" "org"			
			   )))
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
           (type (progn (string-match "\\`\\([^:]*:\\)?\\([^:]*\\)\\(::.*\\)?"
                                      link)
                        (match-string 1 link)))
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
      '(("c" "Simple Agenda view" agenda "")
	("d" todo "DONE")
	("t" todo "TODO")
	("s" todo "SCHEDULED")))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

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
(setq org-agenda-show-all-dates t)
(setq calendar-week-start-day 1)
(setq org-catch-invisible-edits nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 14)
(setq org-log-done t)
(setq org-agenda-files '("/home/ilya/AGENDA/Habits.org" "/home/ilya/AGENDA/Univer.org"))

;; Work with Region
(global-set-key (kbd "<f8>") 'comment-region)
(global-set-key (kbd "C-x w") 'write-region)
