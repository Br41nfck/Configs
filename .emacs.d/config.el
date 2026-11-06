;;; init.el --- Emacs config -*- lexical-binding: t -*-
;; 
;;; Commentary:
;; Пользовательская конфигурация Emacs 31 для Windows.

;;; Code:

;; ---------------------------
;; 1. Пакетная система
;; ---------------------------
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(unless (executable-find "gpg")
  (setq package-check-signature nil))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(eval-when-compile
  (require 'org)
  (require 'calendar)
  (require 'recentf)
  (require 'cc-mode)
  (require 'compile))

(setq use-package-always-ensure t)

;; Файл для custom-настроек
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ---------------------------
;; 2. Основные настройки
;; ---------------------------
(setq default-input-method "russian-computer")

(setq inhibit-startup-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      use-dialog-box nil
      use-short-answers t
      case-fold-search t
      completion-ignore-case t
      ring-bell-function 'ignore
      frame-resize-pixelwise t
      window-resize-pixelwise t
      read-process-output-max (* 1024 1024))

(when (file-directory-p "D:/Projects/")
  (cd "D:/Projects/"))

(setq-default tab-width 4
              indent-tabs-mode nil)

(setq user-full-name "Knedl"
      user-mail-address "georgyinjapan3@gmail.com")

;; ---------------------------
;; 3. Кодировки, локаль и Eshell
;; ---------------------------
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(setq system-time-locale
      (if (eq system-type 'windows-nt) "Russian_Russia" "ru_RU.UTF-8"))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local coding-system-for-read 'utf-8-unix)
            (setq-local coding-system-for-write 'utf-8-unix)
            (setenv "PYTHONIOENCODING" "utf-8")
            (setenv "PYTHONUTF8" "1")
            (setenv "LANG" "en_US.UTF-8")
            (setenv "LC_ALL" "en_US.UTF-8")))

;; ---------------------------
;; 4. Пользовательские функции
;; ---------------------------

(declare-function my/org-all-subtasks-done-p "init")
(declare-function my/org-archive-when-all-done "init")
(declare-function my/org-summary-todo "init")

(defun my/org-insert-src-block (src-code-type)
  "Вставить блок кода в `org-mode' для языка SRC-CODE-TYPE."
  (interactive
   (list (completing-read "Source code type: "
                          '("emacs-lisp" "python" "C" "sh" "C++" "latex" "org"))))
  (insert (format "#+BEGIN_SRC %s\n" src-code-type))
  (insert "#+END_SRC\n"))

(defun my/open-org-dir ()
  "Открыть папку с org-файлами в Dired."
  (interactive)
  (dired org-directory))

(defun my/math-journal-template ()
  "Вставить шаблон математического дневника в текущую позицию."
  (insert "\\section{" (format-time-string "%d-%m-%Y") "}\n\n")
  (insert "\\subsection*{Сегодняшние размышления}\n\n")
  (insert "\\subsection*{Решённые задачи}\n\\begin{itemize}\n\\item \n\\end{itemize}\n\n")
  (insert "\\subsection*{Новые идеи}\n\\begin{itemize}\n\\item \n\\end{itemize}\n\n")
  (insert "\\subsection*{Формулы и вычисления}\n\n"))

(defun my/new-math-journal-entry ()
  "Создать (или открыть) запись математического дневника за сегодня."
  (interactive)
  (let ((filename (format "journal-%s.tex" (format-time-string "%Y-%m-%d"))))
    (find-file (expand-file-name filename user-emacs-directory))
    ;; Шаблон вставляем только в пустой файл
    (when (zerop (buffer-size))
      (my/math-journal-template))))

(defvar my-dictionary-file (expand-file-name "local-dict.txt" user-emacs-directory)
  "Файл со словами пользователя.")
(defvar my-dictionary-cache (make-hash-table :test 'equal)
  "Слова, которые уже есть в словаре.")

(defun my/load-dictionary-cache ()
  "Загрузить существующий словарь в кэш."
  (when (file-exists-p my-dictionary-file)
    (with-temp-buffer
      (insert-file-contents my-dictionary-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (puthash (match-string-no-properties 1) t my-dictionary-cache)))))

(defun my-extract-words ()
  "Вернуть список новых слов из буфера (длиннее 2 букв)."
  (let ((words '())
        (seen (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\<\\([a-zA-Zа-яА-ЯёЁ]+\\)\\>" nil t)
        (let ((word (match-string-no-properties 1)))
          (unless (or (< (length word) 3)
                      (gethash word seen)
                      (gethash word my-dictionary-cache))
            (puthash word t seen)
            (push word words)))))
    (nreverse words)))

(defun my-add-words-to-dictionary (words)
  "Дописать WORDS в файл словаря и в кэш."
  (when words
    (dolist (word words)
      (puthash word t my-dictionary-cache))
    (write-region (concat (mapconcat #'identity words "\n") "\n")
                  nil my-dictionary-file t :silent)))

(add-hook 'after-init-hook #'my/load-dictionary-cache)
(add-hook 'after-save-hook
          (lambda ()
            (when (derived-mode-p 'text-mode)
              (my-add-words-to-dictionary (my-extract-words)))))

(defun my/ibuffer-fullscreen ()
  "Открыть ibuffer на весь экран."
  (interactive)
  (ibuffer)
  (delete-other-windows))

;; ---------------------------
;; 5. Горячие клавиши
;; ---------------------------
(global-set-key (kbd "<C-tab>") #'next-buffer)
(global-set-key (kbd "<C-S-tab>") #'previous-buffer)
(global-set-key (kbd "<f9>") #'compile)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c o") #'my/open-org-dir)
(global-set-key (kbd "C-c g") #'query-replace-regexp)
(global-set-key (kbd "C-c P") #'paradox-list-packages)
(global-set-key (kbd "<f12>") #'save-buffers-kill-emacs)
(global-set-key (kbd "<f8>") #'comment-dwim)
(global-set-key (kbd "<f4>") #'eval-buffer)
(global-set-key (kbd "C-c C-r") #'recentf-open-files)
(global-set-key (kbd "C-c -") #'text-scale-decrease)
(global-set-key (kbd "C-c =") #'text-scale-increase)
(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c j") #'my/new-math-journal-entry)
(global-set-key (kbd "C-x C-b") #'my/ibuffer-fullscreen)

(with-eval-after-load 'compile
  (setq compilation-auto-jump-to-first-error t))

(cua-selection-mode 1)

;; ---------------------------
;; 6. UI
;; ---------------------------
(blink-cursor-mode -1)
(global-display-line-numbers-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(global-visual-line-mode t)
(savehist-mode 1)

;; Шрифт
(when (and (display-graphic-p) (find-font (font-spec :family "Source Code Pro")))
  (set-face-attribute 'default nil :family "Source Code Pro" :height 120))

;; Темы
(use-package doom-themes)
;;(use-package gruber-darker-theme)

(defun my/load-theme (theme)
  "Отключить все активные темы и загрузить тему THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun my/apply-theme-based-on-time ()
  "Тёмная тема с 18:00, светлая днём."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (my/load-theme (if (>= hour 18) 'gruber-darker 'doom-one-light))))

(my/apply-theme-based-on-time)

;; Иконки: современные doom-modeline и dashboard используют nerd-icons.
;; После первой установки выполните M-x nerd-icons-install-fonts
;; и установите скачанный шрифт в Windows вручную.
(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-enable-time t)
  (doom-modeline-time-icon t)
  (doom-modeline-bar-width 2)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-file-name-style 'relative-to-project))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package time
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))

;; Календарь
(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-month-name-array
   ["Январь" "Февраль" "Март" "Апрель" "Май" "Июнь"
    "Июль" "Август" "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"])
  (calendar-day-name-array
   ["Воскресенье" "Понедельник" "Вторник" "Среда" "Четверг" "Пятница" "Суббота"])
  (calendar-day-abbrev-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"])
  (calendar-week-start-day 1)
  (calendar-weekend-days '(0 6))
  (calendar-month-header
   '(propertize (format "%s %d года" monthname year)
                'font-lock-face 'calendar-month-header))
  :config
  (set-face-attribute 'calendar-weekend-header nil :foreground "red")
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (add-hook 'calendar-today-invisible-hook #'calendar-mark-today))

(use-package russian-holidays
  :after calendar
  :config
  (setq holiday-local-holidays russian-holidays
        calendar-holidays holiday-local-holidays))

;; ---------------------------
;; 7. Ввод, автодополнение, поиск
;; ---------------------------
(use-package reverse-im
  :hook (after-init . reverse-im-mode)
  :custom
  (reverse-im-input-methods '("russian-computer")))

(use-package ivy
  :hook (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-show-quick-access t)
  (company-tooltip-limit 14)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate t)
  (company-box-doc-enable t)
  (company-box-max-tooltip-width 50))

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 1
        which-key-idle-secondary-delay 0.5)
  (which-key-setup-minibuffer))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (global-set-key (kbd "M-<up>") #'drag-stuff-up)
  (global-set-key (kbd "M-<down>") #'drag-stuff-down))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never
        recentf-exclude '(".cache" "COMMIT_EDITMSG" "github\\."))
  (recentf-mode 1))

(use-package browse-kill-ring
  :bind ("C-c y" . browse-kill-ring)
  :custom
  (browse-kill-ring-show-preview t))

;; ---------------------------
;; 8. Окна, буферы, проекты
;; ---------------------------
(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package ace-jump-buffer
  :bind (:map goto-map ("b" . ace-jump-buffer)))

(use-package ibuffer
  :ensure nil
  :config
  (setq ibuffer-default-sorting-mode 'recency
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("default"
           ("Org" (mode . org-mode))
           ("Programming C/C++" (or (mode . c-mode) (mode . c++-mode)))
           ("Python" (mode . python-mode))
           ("Emacs-Lisp" (or (mode . emacs-lisp-mode) (name . ".el")))
           ("Text" (name . ".txt")))))
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'auto)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package dashboard
  :init
  (setq inhibit-startup-screen t
        initial-buffer-choice (lambda ()
                                (dashboard-open)
                                (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-items '((recents   . 7)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5))
        dashboard-projects-backend 'projectile
        dashboard-center-content t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts t
        dashboard-banner-logo-title "Добро пожаловать в Emacs, Knedl!"
        dashboard-footer-messages
        '("Ctrl-x Ctrl-f — открыть файл"
          "Ctrl-x b — переключить буфер"
          "Ctrl-c a — открыть Org Agenda"
          "Ctrl-x g — открыть Magit"
          "Alt-x — вызвать команду")))

;; ---------------------------
;; 9. Программирование
;; ---------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((csharp-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :custom
  (lsp-enable-snippet t)
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-signature-auto-activate t)
  (lsp-clients-clangd-args '("--query-driver=C:/mingw64/bin/g++.exe"))
  (lsp-csharp-server-path "omnisharp")
  (lsp-csharp-server-args '("-lsp")))

;; Python запускает lsp только здесь (раньше хук был дважды)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                '(c/c++-clang c/c++-gcc c/c++-cppcheck)))

(use-package web-mode :defer t)
(use-package js2-mode :defer t)
(use-package typescript-mode :defer t)

;; C/C++
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

(use-package google-c-style :defer t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (google-set-c-style)
            (setq-local c-basic-offset 4)))

(add-hook 'c++-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (not (file-exists-p "Makefile"))
                       (not (file-exists-p "makefile")))
              (setq-local compile-command
                          (let* ((file (file-name-nondirectory buffer-file-name))
                                 (exe (file-name-sans-extension file)))
                            (format "g++ -Wall -Wextra -g \"%s\" -o \"%s.exe\" && start \"\" cmd.exe /c \"%s.exe & pause\""
                                    file exe exe))))))

;; Подсветка TODO, BUG, FIXME, NOTE, HACK
(defvar highlight-codetags-keywords
  '(("\\<\\(TODO\\|BUG\\|FIXME\\)\\>" 1 font-lock-warning-face prepend)
    ("\\<\\(NOTE\\|HACK\\)\\>" 1 font-lock-doc-face prepend)))

(define-minor-mode highlight-codetags-local-mode
  "Подсвечивать метки TODO, FIXME и т.п."
  :global nil
  (if highlight-codetags-local-mode
      (font-lock-add-keywords nil highlight-codetags-keywords)
    (font-lock-remove-keywords nil highlight-codetags-keywords))
  (when (bound-and-true-p font-lock-mode)
    (font-lock-flush)))

(add-hook 'prog-mode-hook #'highlight-codetags-local-mode)

(use-package autoinsert
  :ensure nil
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil))

;; Magit + forge
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c m s" . magit-status)
         ("C-c m c" . magit-checkout)
         ("C-c m C" . magit-commit)
         ("C-c m d" . magit-diff)
         ("C-c m b" . magit-blame)
         ("C-c m f" . magit-fetch)
         ("C-c m p" . magit-pull-branch)
         ("C-c m P" . magit-push-current)
         ("C-c m r" . magit-reset)
         ("C-c m R" . magit-rebase)
         ("C-c m l" . magit-log)
         ("C-c m t" . magit-tag)
         ("C-c m T" . magit-tag-delete)
         ("C-c m u" . magit-unstage)
         ("C-c m U" . magit-update-index)
         ("C-c m m" . magit)
         ("C-c m B" . magit-branch)
         ("C-c m M" . magit-merge))
  :custom
  (magit-clone-default-directory (expand-file-name "~/git/")))

(use-package forge
  :after magit)

;; ---------------------------
;; 10. LaTeX
;; ---------------------------
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-engine 'xetex
        TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-default-extension "tex"
        TeX-command-default "LaTeX"
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)
  (setq TeX-view-program-selection
        (pcase system-type
          ('windows-nt '((output-pdf "SumatraPDF")))
          ('darwin     '((output-pdf "Preview")))
          (_           '((output-pdf "Evince"))))))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  ;; Команды модификации (клавиша ` затем ' ): ' + буква
  (setq cdlatex-math-modify-alist
        '((?b "\\mathbb" nil t nil nil)
          (?c "\\mathcal" nil t nil nil)
          (?v "\\overrightarrow" nil t nil nil))))

(use-package company-math
  :hook (LaTeX-mode . (lambda ()
                        (setq-local company-backends
                                    (append '((company-math-symbols-latex
                                               company-latex-commands))
                                            company-backends)))))

;; ---------------------------
;; 11. Org
;; ---------------------------
(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "ORG/" user-emacs-directory))
  (make-directory org-directory t)
  :bind (("<f7>" . org-link-preview)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-display-custom-times t
        org-time-stamp-custom-formats '("<%d-%m-%Y %a %H:%M:%S>" . "<%d-%m-%Y %a %H:%M:%S>")
        org-log-done 'time
        org-startup-folded t
        org-startup-indented t
        org-hide-leading-stars t
        org-ellipsis " ▼"
        org-support-shift-select t
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-archive-location (concat (expand-file-name "Archive.org" org-directory)
                                     "::datetree/"))

  (setq org-capture-templates
        `(("d" "Daily task" entry
           (file+datetree ,(expand-file-name "Daily.org" org-directory))
           "* TODO %?\nSCHEDULED: %t\n")
          ("w" "Weekly Task" entry
           (file+headline ,(expand-file-name "Weekly.org" org-directory) "Inbox")
           "** TODO %?\nDEADLINE: %t")
          ("m" "Monthly Task" entry
           (file+headline ,(expand-file-name "Monthly.org" org-directory) "Inbox")
           "** TODO %?\nDEADLINE: %t")
          ("j" "Journal Entry" entry
           (file+datetree ,(expand-file-name "Journal.org" org-directory))
           "* %?\nEntered on %U\n")))

  (defun my/org-rename-link ()
    "Переименовать файл, на который указывает ссылка в org."
    (interactive)
    (when (org-in-regexp org-link-bracket-re 1)
      (let* ((link (org-link-unescape (match-string-no-properties 1)))
             (filename (file-name-nondirectory link))
             (path (file-name-directory link))
             (newfilename (read-string (format "New name (was '%s'): " filename))))
        (rename-file (concat path filename) (concat path newfilename))
        (replace-match (concat path newfilename) nil nil nil 1))))

  (defun my/org-all-subtasks-done-p ()
    "Вернуть t, если ВСЕ подзадачи текущего заголовка завершены (DONE)."
    (let ((all-done t))
      (org-map-entries
       (lambda ()
         (let ((state (org-get-todo-state)))
           (when (and state (not (string= state "DONE")))
             (setq all-done nil))))
       nil 'tree)
      all-done))

  (defun my/org-archive-when-all-done ()
    "Архивировать задачу 4 уровня, если все подзадачи внутри неё выполнены."
    (when (and (string= org-state "DONE")
               (= (org-current-level) 4)
               (my/org-all-subtasks-done-p))
      (message "Все подзадачи выполнены — архивирую %s" (org-get-heading t t t t))
      (org-archive-subtree)
      (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))))

  (add-hook 'org-after-todo-state-change-hook #'my/org-archive-when-all-done)

  (defun my/org-summary-todo (_n-done n-not-done)
    "Менять состояние родителя на DONE, когда все подзадачи завершены."
    (let ((org-log-done nil))
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ---------------------------
;; 12. Разное
;; ---------------------------
(use-package paradox
  :config
  (setq paradox-github-token t))

(use-package clipmon
  :hook (after-init . clipmon-mode-start))

(use-package transmission
  :bind ("C-x a t" . transmission))

(use-package eww
  :ensure nil
  :custom
  (eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="))

(use-package browse-url
  :ensure nil
  :bind ([f5] . browse-url))

(use-package webjump
  :ensure nil
  :bind ([S-f5] . webjump)
  :config
  (setq webjump-sites
        (append '(("debian packages" .
                   [simple-query "packages.debian.org" "http://packages.debian.org/" ""]))
                webjump-sample-sites)))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 7
        auto-package-update-prompt-before-update t)
  (run-with-idle-timer 5 nil #'auto-package-update-maybe))

(message "Config File Reloaded!")

;;; init.el ends here
