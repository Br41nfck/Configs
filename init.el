;;; init.el --- Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; Пользовательская конфигурация Emacs для ОС Windows.

;;; Code:

;; ---------------------------
;; 1. Базовые пути и переменные
;; ---------------------------
;; Важно задать user-emacs-directory до инициализации пакетов
(setq user-emacs-directory (expand-file-name "~/.config/emacs/"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Устранение предупреждений компилятора для переменных, используемых до загрузки пакетов
(defvar eshell-coding-system)
(defvar compilation-auto-jump-to-first-error)
(defvar calendar-holidays)
(defvar holiday-local-holidays)

;; Ускорение загрузки (GC и file-name-handler)
(defvar my/default-gc-cons-threshold 16777216)
(defvar my/default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/default-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/default-file-name-handler-alist)))

;; ---------------------------
;; 2. Пакетная система
;; ---------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Автоматическая установка недостающих пакетов
(require 'seq)
(add-hook 'after-init-hook
          (lambda ()
            (when (boundp 'package-selected-packages)
              (let ((missing-packages (seq-remove #'package-installed-p package-selected-packages)))
                (when missing-packages
                  (message "Скачивание и установка недостающих пакетов на новом ПК...")
                  (package-refresh-contents)
                  (mapc #'package-install missing-packages)
                  (message "Все пакеты успешно установлены!"))))))

;; Настройка файла для custom-настроек
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ---------------------------
;; 3. Основные настройки
;; ---------------------------
(setq inhibit-startup-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      use-dialog-box nil
      case-fold-search t
      completion-ignore-case t
      default-directory "D:/Projects/") ;; Windows-путь

;; Tabulation: indent-tabs-mode - это логическая переменная (t или nil), а не число!
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) 

;; ---------------------------
;; 4. Кодировки и Eshell
;; ---------------------------
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq eshell-coding-system 'utf-8-unix
                  coding-system-for-read 'utf-8-unix
                  coding-system-for-write 'utf-8-unix)
            ;; Переменные окружения для Python
            (setenv "PYTHONIOENCODING" "utf-8")
            (setenv "PYTHONUTF8" "1")
            (setenv "LANG" "en_US.UTF-8")
            (setenv "LC_ALL" "en_US.UTF-8")
            (message "Eshell: UTF-8 mode enabled")))

;; ---------------------------
;; 5. Пользовательские функции
;; ---------------------------
(defun my/org-insert-src-block (src-code-type)
  "Вставить блок кода в `org-mode' для языка SRC-CODE-TYPE."
  (interactive
   (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "C++" "latex" "org")))
     (list (completing-read "Source code type: " src-code-types)))) ; Заменено ivy-completing-read на универсальный completing-read
  (insert (format "#+BEGIN_SRC %s\n" src-code-type))
  (insert "#+END_SRC\n"))

(with-eval-after-load 'org
  (defun my/org-rename-link ()
    "Переименовать файл, на который указывает ссылка в org."
    (interactive)
    (when (org-in-regexp org-link-bracket-re 1)
      (let* ((link (org-link-unescape (match-string-no-properties 1)))
             (filename (file-name-nondirectory link))
             (path (file-name-directory link))
             (newfilename (read-string (format "New name (was '%s'): " filename))))
        (rename-file (concat path filename) (concat path newfilename))
        (replace-match (concat path newfilename) nil nil nil 1)))))

(defun my/open-org-dir ()
  "Открыть папку с моими org-файлами в Dired."
  (interactive)
  (dired (expand-file-name "ORG" user-emacs-directory)))

(defun my/get-main-journal-path ()
  "Try to find a main-journal.tex in project or user dir."
  (let ((candidates (list (expand-file-name "main-journal.tex" user-emacs-directory)
                          (expand-file-name "journal/main-journal.tex" user-emacs-directory))))
    (seq-find #'file-exists-p candidates)))

(defun my/math-journal-template ()
  "Insert math journal template at point."
  (insert "\\section{" (format-time-string "%d-%m-%Y") "}\n\n")
  (insert "\\subsection*{Сегодняшние размышления}\n\n")
  (insert "\\subsection*{Решённые задачи}\n\\begin{itemize}\n\\item \n\\end{itemize}\n\n")
  (insert "\\subsection*{Новые идеи}\n\\begin{itemize}\n\\item \n\\end{itemize}\n\n")
  (insert "\\subsection*{Формулы и вычисления}\n\n"))

(defun my/new-math-journal-entry ()
  "Create new math journal entry file."
  (interactive)
  (let ((filename (format "journal-%s.tex" (format-time-string "%Y-%m-%d"))))
    (find-file (expand-file-name filename user-emacs-directory))
    (my/math-journal-template)))

;; Словарь
(defvar my-dictionary-file (expand-file-name "local-dict.txt" user-emacs-directory)
  "File to store user words.")
(defvar my-dictionary-cache (make-hash-table :test 'equal))

(defun my-extract-words ()
  "Extract words from buffer."
  (let ((words '())
        (seen (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\<\\([a-zA-Zа-яА-ЯёЁ]+\\)\\>" nil t)
        (let ((word (match-string-no-properties 1)))
          (unless (or (gethash word seen)
                      (gethash word my-dictionary-cache)
                      (< (length word) 3))
            (puthash word t seen)
            (push word words)))))))

(defun my-add-words-to-dictionary (words)
  "Add new WORDS to dictionary file."
  (when words
    (let ((new-words '()))
      (when (file-exists-p my-dictionary-file)
        (with-temp-buffer
          (insert-file-contents my-dictionary-file)
          (dolist (word words)
            (unless (or (search-forward word nil t)
                        (gethash word my-dictionary-cache))
              (push word new-words)
              (puthash word t my-dictionary-cache)))))
      (when new-words
        (with-temp-buffer
          (when (file-exists-p my-dictionary-file)
            (insert-file-contents my-dictionary-file))
          (dolist (word new-words)
            (goto-char (point-max))
            (insert word "\n"))
          (write-region (point-min) (point-max) my-dictionary-file nil :silent))))))

(defun my/load-dictionary-cache ()
  "Load existing dictionary into cache."
  (when (file-exists-p my-dictionary-file)
    (with-temp-buffer
      (insert-file-contents my-dictionary-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (puthash (match-string-no-properties 1) t my-dictionary-cache)))))

(add-hook 'after-init-hook #'my/load-dictionary-cache)
(add-hook 'after-save-hook
          (lambda ()
            (when (derived-mode-p 'text-mode 'prog-mode)
              (my-add-words-to-dictionary (my-extract-words)))))

;; ---------------------------
;; 6. Горячие клавиши
;; ---------------------------
(global-set-key (kbd "<C-tab>") #'next-buffer)
(global-set-key (kbd "<C-S-tab>") #'previous-buffer)

(global-set-key (kbd "<f9>") #'compile)
(setq compilation-auto-jump-to-first-error t)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c o") #'my/open-org-dir)
(global-set-key (kbd "C-c g") #'query-replace-regexp)
(global-set-key (kbd "C-c p") #'paradox-list-packages)
(global-set-key (kbd "<f12>") #'save-buffers-kill-emacs)
(global-set-key (kbd "<f8>") #'comment-dwim)
(global-set-key (kbd "<f4>") #'eval-buffer)
(global-set-key (kbd "C-c C-r") #'recentf-open-files)
(global-set-key (kbd "C-c -") #'text-scale-decrease)
(global-set-key (kbd "C-c =") #'text-scale-increase)
(global-set-key (kbd "C-s") #'swiper)
(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c j") #'my/new-math-journal-entry)

(cua-selection-mode 1) 

;; ---------------------------
;; 7. Конфигурация пакетов (use-package)
;; ---------------------------
(use-package drag-stuff
  :config
  (drag-stuff-mode 1)
  (global-set-key (kbd "M-<up>") #'drag-stuff-up)
  (global-set-key (kbd "M-<down>") #'drag-stuff-down))

(use-package paradox
  :config
  (setq paradox-github-token "<github-token>"))

(use-package autoinsert
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (setq auto-insert-alist
        (append '(("\\.el\\'" . [nil
                                 ";;; %n.el --- Summary\n\n"
                                 ";;; Commentary:\n\n"
                                 ";;; Code:\n"
                                 ";;; lexical-binding: t\n\n"
                                 "(defun %n-setup ()\n"
                                 "  \"Setup function for %n.\"\n"
                                 "  (interactive))\n\n"
                                 "(provide '%n)\n\n"
                                 ";;; %n.el ends here\n"]))
                auto-insert-alist)))

(use-package recentf
  :config
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never
        recentf-exclude '(".cache" "COMMIT_EDITMSG" "github\\."))
  (recentf-mode 1))

(use-package browse-kill-ring
  :bind (("C-c y" . browse-kill-ring))
  :custom
  (browse-kill-ring-show-preview t))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
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
  (cond
   ((eq system-type 'gnu/linux)
    (setq TeX-view-program-selection '((output-pdf "Evince"))))
   ((eq system-type 'windows-nt)
    (setq TeX-view-program-selection '((output-pdf "Sumatra"))))
   ((eq system-type 'darwin)
    (setq TeX-view-program-selection '((output-pdf "Preview"))))))

(use-package cdlatex
  :hook (latex-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-math-symbol-alist
        '((?b ("\\mathbb" "\\mathbb{}" cdlatex-position-cursor))
          (?c ("\\mathcal" "\\mathcal{}" cdlatex-position-cursor))
          (?v ("\\overrightarrow" "\\overrightarrow{}" cdlatex-position-cursor))
          (?^ ("\\hat" "\\hat{}" cdlatex-position-cursor))
          (?2 ("^2" "" nil))
          (?3 ("^3" "" nil)))))

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package company-math
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package org
  :init
  (setq org-directory (expand-file-name "ORG" user-emacs-directory))
  :bind (("<f7>" . org-toggle-inline-images)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (set-locale-environment "ru_RU.UTF-8")
  (setq system-time-locale "ru_RU.UTF-8"
        org-display-custom-times t
        org-time-stamp-custom-formats '("<%d-%m-%Y %a %H:%M:%S>" . "<%d-%m-%Y %a %H:%M:%S>")
        org-log-done 'time
        org-startup-folded t
        org-startup-indented t
        org-hide-leading-stars t
        org-ellipsis " ▼"
        org-support-shift-select t
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-archive-location (concat org-directory "Archive.org::datetree/"))
  
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
    "Архивировать задачу только если все подзадачи внутри неё выполнены."
    (when (and (string= org-state "DONE")
               (= (org-current-level) 4)
               (my/org-all-subtasks-done-p))
      (message "Все подзадачи выполнены — архивирую %s" (org-get-heading t t t t))
      (org-archive-subtree)
      (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))))

  (remove-hook 'org-after-todo-state-change-hook 'my/org-archive-done-tasks)
  (add-hook 'org-after-todo-state-change-hook #'my/org-archive-when-all-done)

  (defun my/org-summary-todo (_n-done n-not-done)
    "Автоматически меняет состояние родителя на DONE, когда все подзадачи завершены."
    (let ((org-log-done nil))
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Шрифты
(when (member "Source Code Pro" (font-family-list))
  (set-fontset-font t 'unicode (font-spec :family "Source Code Pro" :size 14) nil 'prepend)
  (set-frame-font "Source Code Pro-16" t t)
  (set-fontset-font t 'cyrillic (font-spec :family "Source Code Pro" :size 22) nil 'prepend))

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq user-full-name "Knedl"
      user-login-name "Knedl"
      user-mail-address "georgyinjapan3@gmail.com")

;; Темы
(defun my/load-theme (theme)
  "Отключить все активные темы и загрузить тему THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun my/apply-theme-based-on-time ()
  "Применить светлую или темную тему в зависимости от времени суток."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (>= hour 18)
        (my/load-theme 'gruber-darker)
      (my/load-theme 'doom-one-light))))
(my/load-theme 'gruber-darker)

;; Время
(use-package time
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))

;; Календарь
(use-package calendar
  :defer t
  :custom
  (calendar-month-name-array
   ["Январь" "Февраль" "Март" "Апрель" "Май" "Июнь"
    "Июль" "Август" "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"])
  (calendar-day-name-array
   ["Воскресенье" "Понедельник" "Вторник" "Среда" "Четверг" "Пятница" "Суббота"])
  (calendar-day-abbrev-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"])
  (calendar-week-start-day 1)
  (calendar-weekend '(0 6))
  (calendar-month-header-function
   (lambda (month year)
     (format "%s %d года"
             (aref calendar-month-name-array (1- month))
             year)))
  :config
  (copy-face 'font-lock-keyword-face 'calendar-weekend-header)
  (set-face-attribute 'calendar-weekend-header nil :foreground "red")
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (add-hook 'calendar-today-invisible-hook 'calendar-mark-today)
  (use-package russian-holidays
    :after calendar
    :config
    (setq holiday-local-holidays russian-holidays)
    (setq calendar-holidays holiday-local-holidays)))

;; UI и статус-бар
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

(use-package all-the-icons)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ;; Правильный способ включения глобально

(global-display-line-numbers-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(global-visual-line-mode t)
(savehist-mode 1)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)
;; Навигация, окна, буферы
(use-package ace-window
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                   aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package ace-jump-buffer
  :bind (:map goto-map ("b" . ace-jump-buffer)))

(use-package ibuffer
  :bind ("C-x C-b" . my/ibuffer-fullscreen)
  :config
  (defun my/ibuffer-fullscreen ()
    "Открыть ibuffer на весь экран."
    (interactive)
    (ibuffer)
    (delete-other-windows))
  (setq ibuffer-default-sorting-mode 'recency
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("default"
           ("Org" (mode . org-mode))
           ("Programming C/C++" (or (mode . c-mode) (mode . c++-mode)))
           ("Python" (mode . python-mode))
           ("Emacs-Lisp" (or (mode . emacs-lisp-mode) (name . ".el")))
           ("Text" (name . ".txt")))))
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

;; LSP и автодополнение
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (csharp-mode . lsp-deferred)
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

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil))

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
  (company-dabbrev-ignore-case t)
  :config
  (add-to-list 'company-backends 'company-capf))

;; C/CPP Mode
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c-mode-hook  #'rainbow-delimiters-mode)

;; Highlights TODO, BUG, NOTE, HACK, FIXME
(defvar highlight-codetags-keywords
  '(("\\<\\(TODO\\|BUG\\|FIXME\\)\\>" 1 font-lock-warning-face prepend)
    ("\\<\\(NOTE\\|HACK\\)\\>" 1 font-lock-doc-face prepend)))

(define-minor-mode highlight-codetags-local-mode
  "Highlight codetags like TODO, FIXME..."
  :global nil
  (if highlight-codetags-local-mode ;; ИСПРАВЛЕНО: было highlight=codetags-local-mode
      (font-lock-add-keywords nil highlight-codetags-keywords)
    (font-lock-remove-keywords nil highlight-codetags-keywords))
  (when (bound-and-true-p font-lock-mode)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'prog-mode-hook #'highlight-codetags-local-mode)

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

(add-hook 'c++-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (not (file-exists-p "Makefile"))
                       (not (file-exists-p "makefile")))
              (set (make-local-variable 'compile-command)
                   (let* ((file (file-name-nondirectory buffer-file-name))
                          (exe (file-name-sans-extension file)))
                     (format "g++ -Wall -Wextra -g \"%s\" -o \"%s.exe\" && start \"\" cmd.exe /c \"%s.exe & pause\""
                             file exe exe))))))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate t)
  (company-box-doc-enable t)
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-backends-colors nil)
  (company-box-max-tooltip-width 50))

;; Magit + forge
(use-package magit
  :custom
  (magit-clone-default-directory (expand-file-name "~/git/"))
  (magit-completing-read-function 'ivy-completing-read)
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
         ("C-c m M" . magit-merge)))

(use-package forge
  :after magit)

;; Extras
(use-package clipmon
  :hook (after-init . clipmon-mode-start))

(use-package reverse-im
  :after char-fold
  :hook (after-init . reverse-im-mode)
  :custom
  (reverse-im-input-methods '("russian-computer"))
  (reverse-im-read-char-advice-function #'reverse-im-read-char-exclude))

(use-package transmission
  :bind (("C-x a t" . transmission)))

(use-package eww
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

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 1
        which-key-idle-secondary-delay 0.5)
  (which-key-setup-minibuffer))

(use-package dashboard
  :init
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice (lambda ()
                                (dashboard-open)
                                (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents   . 7)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5))
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts t)
  (setq dashboard-banner-logo-title "Добро пожаловать в Emacs, Knedl!")
  (setq dashboard-footer-messages
        '("Ctrl-x Ctrl-f — открыть файл"
          "Ctrl-x b — переключить буфер"
          "Ctrl-c a — открыть Org Agenda"
          "Ctrl-x g — открыть Magit"
          "Alt-x — вызвать команду"))
  (setq dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (projects  . "rocket")
                                  (agenda    . "calendar"))))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'auto)
  (define-key projectile-mode-map (kbd "C-c p m") 'projectile-command-map))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 7
        auto-package-update-prompt-before-update t)
  (run-with-idle-timer 5 nil #'auto-package-update-maybe))

(message "Config File Reloaded!")

;; ---------------------------
;; 8. Custom-переменные
;; ---------------------------
(custom-set-variables
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     default))
 '(package-selected-packages
   '(ac-math ace-jump-buffer ace-link ace-window achievements all-the-icons auctex auto-package-update bluetooth calfw-org cape cdlatex clang-format clipmon company-box company-math corfu counsel cuda-mode dashboard diminish docker docker-compose-mode dockerfile-mode doom-modeline doom-themes drag-stuff flycheck forge google-translate helpful highlight-numbers info-colors kind-icon lsp-ui multiple-cursors orderless org-bullets page-break-lines paradox pomidor projectile rainbow-delimiters rainbow-identifiers rainbow-mode reverse-im russian-calendar russian-holidays smartparens sudo-edit transmission web-mode yasnippet)))

(custom-set-faces)
;;; init.el ends here
