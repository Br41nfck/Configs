;;; init.el --- Emacs config -*- lexical-binding: t; -*-

;; ---------------------------
;; Основные настройки
;; ---------------------------
(setq user-emacs-directory "~/.config/emacs/"
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      use-dialog-box nil)

;; Расположение кастомного файла
(require 'custom)
(customize-set-variable 'custom-file
                        (expand-file-name "init.el" user-emacs-directory)
                        "Файл для сохранения пользовательских настроек, сделанных в customize.")

;; ---------------------------
;; Пути к кастомным файлам
;; ---------------------------
(use-package my-keybinds
  :load-path "~/.config/emacs/lisp/"
  :demand t)

(use-package my-functions
  :load-path "~/.config/emacs/lisp/"
  :demand t)

;; ---------------------------
;; Пакетная система
;; ---------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------
;; Шрифты
;; ---------------------------
(set-fontset-font t 'unicode (font-spec :family "FiraCode Nerd Font" :size 14) nil 'prepend)
(set-frame-font "FiraCode Nerd Font-16" t t)
(set-fontset-font t 'cyrillic (font-spec :family "FiraCode Nerd Font" :size 22) nil 'prepend)

;; ---------------------------
;; UI
;; ---------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
;(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq user-full-name "Knedl"
      user-login-name "Knedl"
      user-mail-address "georgyinjapan3@gmail.com")

;; ---------------------------
;; Темы
;; ---------------------------
(defun my/load-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun my/apply-theme-based-on-time ()
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (>= hour 18)
        (my/load-theme 'doom-one)
      (my/load-theme 'doom-one-light))))

(my/apply-theme-based-on-time)
(run-at-time "00:00" 3600 #'my/apply-theme-based-on-time)

;; ---------------------------
;; Время
;; ---------------------------
(use-package time
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))

;; ---------------------------
;; Календарь
;; ---------------------------
(use-package calendar
  :defer t
  :config
  (setq calendar-month-name-array
        ["Январь" "Февраль" "Март" "Апрель" "Май" "Июнь"
         "Июль" "Август" "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"])
  (setq calendar-day-name-array
        ["Воскресенье" "Понедельник" "Вторник" "Среда" "Четверг" "Пятница" "Суббота"])
  (setq calendar-day-abbrev-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"])
  (setq calendar-week-start-day 1
        calendar-weekend '(0 6))
  (copy-face 'font-lock-keyword-face 'calendar-weekend-header)
  (set-face-attribute 'calendar-weekend-header nil :foreground "red")
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (add-hook 'calendar-today-invisible-hook 'calendar-mark-today)
  (setq calendar-month-header-function
        (lambda (month year)
          (format "%s %d года"
                  (aref calendar-month-name-array (1- month))
                  year)))
  ;; Русские праздники
  (use-package russian-holidays
    :after calendar
    :config
    (setq holiday-local-holidays russian-holidays
          calendar-holidays holiday-local-holidays)))

;; ---------------------------
;; Org-mode
;; ---------------------------
(setq diary-file (expand-file-name "~/.config/emacs/diary"))
(unless (file-exists-p diary-file)
  (with-temp-buffer (write-file diary-file)))

(use-package org
  :ensure t
  :init
  (setq org-directory "~/.config/emacs/ORG/")
  :bind (("<f7>" . org-toggle-inline-images)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (set-locale-environment "ru_RU.UTF-8")
  (setq system-time-locale "ru_RU.UTF-8"
        org-display-custom-times t
        org-time-stamp-custom-formats '("<%d-%m-%Y %a %H:%M:%S>" . "<%d-%m-%Y %a %H:%M:%S>")
        org-log-done 'time)

  ;; Файлы и архив
  (setq org-startup-folded t
        org-support-shift-select t
        org-agenda-files (directory-files-recursively org-directory  "\\.org$")
        org-archive-location (concat org-directory "Archive.org::datetree/"))

  
  ;; Capture templates (C-c c d/w/m/j)
  (setq org-capture-templates
        `(("d" "Daily task" entry
           (file+datetree ,(concat org-directory "Daily.org"))
           "* TODO %?\nSCHEDULED: %U\n")
          ("w" "Weekly Task" entry
           (file+headline ,(concat org-directory "Weekly.org") "Inbox")
           "** TODO %?\nDEADLINE: %U")
          ("m" "Monthly Task" entry
           (file+headline ,(concat org-directory "Monthly.org") "Inbox")
           "** TODO %?\nDEADLINE: %U")
          ("j" "Journal Entry" entry
           (file+datetree ,(concat org-directory "Journal.org"))
           "* %?\nEntered on %U\n")))

  ;; ---------------------------
;; Умное архивирование только после завершения всех подзадач
;; ---------------------------

(defun my/org-all-subtasks-done-p ()
  "Возвращает t, если ВСЕ подзадачи текущего заголовка завершены (DONE)."
  (let ((all-done t))
    (org-map-entries
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when (and state (not (string= state "DONE")))
           (setq all-done nil))))
     nil 'tree)
    all-done))

(defun my/org-archive-when-all-done ()
  "Архивировать задачу только если все подзадачи внутри неё выполнены.
Не архивирует отдельные подзадачи."
  (when (and (string= org-state "DONE")
             ;; проверяем, что текущая задача не подзадача
             (= (org-current-level) 4)
             ;; и что внутри неё все дочерние задачи DONE
             (my/org-all-subtasks-done-p))
    (message "Все подзадачи выполнены — архивирую %s" (org-get-heading t t t t))
    (org-archive-subtree)
    (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))))

;; Убираем старый автoархив
(remove-hook 'org-after-todo-state-change-hook #'my/org-archive-done-tasks)
;; Добавляем новый
(add-hook 'org-after-todo-state-change-hook #'my/org-archive-when-all-done)

(defun my/org-summary-todo (n-done n-not-done)
  "Автоматически меняет состояние родителя на DONE, когда все подзадачи завершены."
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo)

  
;; Красивые bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)))

;; ---------------------------
;; UI и статус-бар
;; ---------------------------
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-enable-time t
        doom-modeline-time-icon t
        doom-modeline-bar-width 2
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-file-name-style 'relative-to-project))

(use-package all-the-icons)

(global-display-line-numbers-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(global-hl-line-mode t)
(global-visual-line-mode t)
(savehist-mode 1)

;; ---------------------------
;; Навигация, окна, буферы
;; ---------------------------
(use-package ace-window
  :ensure t
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                   aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package ace-jump-buffer
  :ensure t
  :bind (:map goto-map ("b" . ace-jump-buffer)))

(use-package ibuffer
  :ensure nil
  :config
  (setq ibuffer-default-sorting-mode 'major-mode
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("default"
           ("Org" (mode . org-mode))
           ("Programming C/C++" (or (mode . c-mode) (mode . c++-mode)))
           ("Python" (mode . python-mode))
           ("Emacs-Lisp" (or (mode . emacs-lisp) (name . ".el")))
           ("Text" (name . ".txt")))))
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

;; ---------------------------
;; LSP и автодополнение
;; ---------------------------
(use-package lsp-mode
  :hook ((js-mode js2-mode typescript-mode web-mode css-mode html-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-capf t
        lsp-enable-snippet t
        lsp-completion-provider :capf))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil))

(use-package company
  :init (global-company-mode 1)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-show-quick-access t
        company-tooltip-limit 14
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-doc-enable t
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil
        company-box-max-tooltip-width 50))

;; ---------------------------
;; Magit + Forge
;; ---------------------------
(use-package magit
  :ensure t
  :custom
  (magit-clone-default-directory (expand-file-name "~/git/"))
  (magit-completing-read-function 'ivy-completing-read)
  :bind (:map mode-specific-map
              :prefix-map magit-prefix-map
              :prefix "m"
              (("s" . magit-status)
               ("c" . magit-checkout)
               ("C" . magit-commit)
               ("d" . magit-diff)
               ("b" . magit-blame)
               ("f" . magit-fetch)
               ("p" . magit-pull-branch)
               ("P" . magit-push-current)
               ("r" . magit-reset)
               ("R" . magit-rebase)
               ("l" . magit-log)
               ("t" . magit-tag)
               ("T" . magit-tag-delete)
               ("u" . magit-unstage)
               ("U" . magit-update-index)
               ("m" . magit)
               ("B" . magit-branch)
               ("M" . magit-merge))))

(use-package forge
  :after magit)

;; ---------------------------
;; Clipmon, Reverse-im, Transmission
;; ---------------------------
(use-package clipmon
  :ensure t
  :hook (after-init . clipmon-mode-start))

(use-package reverse-im
  :after char-fold
  :custom
  (reverse-im-input-methods '("russian-computer"))
  (reverse-im-read-char-advice-function #'reverse-im-read-char-exclude)
  :config (reverse-im-mode t))

(use-package transmission
  :bind (("C-x a t" . transmission)))

;; ---------------------------
;; EWW, Webjump
;; ---------------------------
(use-package eww
  :defer t
  :custom
  (eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="))

(use-package browse-url
  :bind ([f5] . browse-url))

(use-package webjump
  :bind ([S-f5] . webjump)
  :config
  (setq webjump-sites
        (append '(("debian packages" .
                   [simple-query "packages.debian.org" "http://packages.debian.org/" ""]))
                webjump-sample-sites)))

;; ---------------------------
;; Elfeed
;; ---------------------------

;;(use-package elfeed
;;  :bind (("C-x w" . elfeed))
;; :config
;;  (setq elfeed-search-title-max-width 120
;;        elfeed-search-filter "@1-week-ago +unread"))

;; ---------------------------
;; Which-key
;; ---------------------------
(use-package which-key
  :ensure t
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t
        ;; Don't show automatically, but refresh quickly after C-h
        which-key-idle-delay 1
        which-key-idle-secondary-delay 0.5)
  ;; Enable which-key globally
  (which-key-mode)
  ;; Display in minibuffer (alternative: side-window-bottom/right)
  (which-key-setup-minibuffer))

;; ---------------------------
;; Dashboard (в стиле Doom Emacs)
;; ---------------------------
(use-package dashboard
  :ensure t
  :init
  ;; Показывать dashboard при запуске
  (setq inhibit-startup-screen t
        initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook)
  :config
  ;; Баннер (логотип)
  (setq dashboard-startup-banner 'logo) ;; варианты: 'official, 'logo, путь к png/jpg
  
  ;; Основные элементы
  (setq dashboard-items '((recents   . 7)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5))
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts t)

  ;; Красивый заголовок
  (setq dashboard-banner-logo-title "Добро пожаловать в Emacs, Knedl!")

  ;; Нижний текст
  (setq dashboard-footer-messages
        '("Ctrl-x Ctrl-f — открыть файл"
          "Ctrl-x b — переключить буфер"
          "Ctrl-c a — открыть Org Agenda"
          "Ctrl-x g — открыть Magit"
          "Alt-x — вызвать команду"))

  ;; Символы для заголовков
  (setq dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (projects  . "rocket")
                                  (agenda    . "calendar")))

  ;; Поддержка `projectile` для списка проектов
  (use-package projectile
    :ensure t
    :config
    (projectile-mode +1)
    (setq projectile-completion-system 'auto)))

;; ---------------------------
;; Авто-обновление пакетов
;; ---------------------------
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 7
        auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

(message "Config File Reloaded!")
;; ---------------------------
;; Завершение
;; ---------------------------
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ac-math ace-jump-buffer ace-link ace-window achievements
	     all-the-icons auctex auto-package-update bluetooth
	     calfw-org cape cdlatex clang-format clipmon company-box
	     company-math corfu counsel cuda-mode dashboard diminish
	     docker docker-compose-mode dockerfile-mode doom-modeline
	     doom-themes flycheck forge google-translate helpful
	     highlight-numbers info-colors kind-icon lsp-ui
	     multiple-cursors orderless org-bullets page-break-lines
	     paradox pomidor projectile rainbow-delimiters
	     rainbow-identifiers rainbow-mode reverse-im
	     russian-calendar russian-holidays smartparens sudo-edit
	     transmission web-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
