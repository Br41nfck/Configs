;;; init.el --- Точка входа -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Ускоряем загрузку
(setq gc-cons-threshold (* 50 1000 1000))
(setq package-quickstart t)

;; Пути
(setq user-emacs-directory "~/.emacs.d/")

;; Пакетный менеджер
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Установка use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;------------------------------------------------------------------------------
;; Auto Insert — Шаблоны для новых файлов
;;------------------------------------------------------------------------------
(use-package autoinsert
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (setq auto-insert-alist
        (append '(
                  ("\\.el\\'" . [nil
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

;;------------------------------------------------------------------------------
;; Fonts — Установка и применение шрифта
;;------------------------------------------------------------------------------
(defun my/install-source-code-pro ()
  "Установить Source Code Pro на Windows."
  (let* ((font-name "Source Code Pro")
         (fonts-dir (expand-file-name "fonts" user-emacs-directory))
         (zip-url "https://github.com/adobe-fonts/source-code-pro/archive/refs/heads/release.zip")
         (zip-file (expand-file-name "source-code-pro.zip" fonts-dir))
         (install-dir (expand-file-name "source-code-pro-release/TTF" fonts-dir)))
    (unless (member font-name (font-family-list))
      (unless (file-exists-p install-dir)
        (make-directory fonts-dir t)
        (url-copy-file zip-url zip-file t)
        (shell-command (format "powershell -command \"Expand-Archive -Path '%s' -DestinationPath '%s' -Force\"" zip-file fonts-dir)))
      (let ((default-directory install-dir))
        (dolist (file (directory-files install-dir t "\\.ttf$"))
          (shell-command (format "powershell -command \"Copy-Item '%s' -Destination 'C:\\Windows\\Fonts' -Force\"" file))))))
  (message "Source Code Pro установлен."))

(when (eq system-type 'windows-nt)
  (add-hook 'emacs-startup-hook #'my/install-source-code-pro))

;; Применить шрифт
(when (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil :family "Source Code Pro" :height 140))

;;------------------------------------------------------------------------------
;; UI — Интерфейс
;;------------------------------------------------------------------------------
(eval-when-compile
  (defvar display-line-numbers-type)
  (defvar display-line-numbers-width-start)
  (defvar display-line-numbers-exclude-modes)
  (defvar savehist-additional-variables)
  (defvar global-hl-line-sticky-flag))

;; Отключаем лишнее
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Нумерация строк
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(unless (bound-and-true-p display-line-numbers-exclude-modes)
  (setq display-line-numbers-exclude-modes '(minibuffer-inactive-mode)))

(dolist (mode '(org-mode term-mode shell-mode eshell-mode vterm-mode
                        dired-mode help-mode messages-buffer-mode))
  (add-to-list 'display-line-numbers-exclude-modes mode))

(setq display-line-numbers-width-start t)

;; Тема
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Сохранение позиций
(save-place-mode 1)
(savehist-mode 1)
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))

;; Recentf
(use-package recentf
  :config
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never
        recentf-exclude '(".cache" "COMMIT_EDITMSG" "github\\."))
  (recentf-mode 1))

;; Clipboard History
(use-package browse-kill-ring
  :ensure t
  :bind ("C-c y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-show-preview t))

;; Bookmarks
(use-package bookmark
  :init (setq bookmark-save-flag 1)
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-automatically-show-annotations t)
  (global-set-key (kbd "C-x r b") 'bookmark-jump)
  (global-set-key (kbd "C-x r m") 'bookmark-set)
  (global-set-key (kbd "C-x r l") 'bookmark-bmenu-list))

;; My Dictionary
(defvar my-dictionary-file (expand-file-name "local-dict.txt" user-emacs-directory)
  "Файл для хранения пользовательских слов.")

(defun my-extract-words ()
  "Извлечь слова из текущего буфера."
  (let ((words '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([a-zA-Zа-яА-Я]+\\)\\b" nil t)
        (let ((word (match-string-no-properties 1)))
          (unless (member word words)
            (push word words)))))
    words))

(defun my-add-words-to-dictionary (words)
  "Добавить слова в словарь."
  (when words
    (with-temp-buffer
      (when (file-exists-p my-dictionary-file)
        (insert-file-contents my-dictionary-file))
      (dolist (word words)
        (unless (search-forward word nil t)
          (goto-char (point-max))
          (insert word "\n")))
      (write-region (point-min) (point-max) my-dictionary-file))))

(add-hook 'after-save-hook
          (lambda () (my-add-words-to-dictionary (my-extract-words))))

;; Визуальные улучшения
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(global-hl-line-mode 1)
(setq global-hl-line-sticky-flag nil)

(setq indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)


;;------------------------------------------------------------------------------
;; Which-key - Показывает комбинации клавиш
;;------------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 1     ; задержка перед показом (сек)
        which-key-side-window-max-width 0.33
        which-key-side-window-location 'bottom ; 'right, 'left, 'bottom
        which-key-popup-type 'side-window ; 'minibuffer, 'side-window
        which-key-min-display-lines 6
        which-key-echo-area-message-format nil ; отключить echo
        which-key-add-column-padding 1
        which-key-max-description-length 25
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-prefixes nil ; не показывать префиксы в начале
        )
  :diminish which-key-mode)

;;------------------------------------------------------------------------------
;; Keys — Глобальные клавиши
;;------------------------------------------------------------------------------

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f12>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "<f4>") 'eval-buffer)

(defun my/toggle-paren-style ()
  "Переключить стиль show-paren между 'expression и 'parenthesis."
  (interactive)
  (if (eq show-paren-style 'expression)
      (setq show-paren-style 'parenthesis)
    (setq show-paren-style 'expression))
  ;; Перезапускаем режим, чтобы изменения вступили в силу
  (when show-paren-mode
    (show-paren-mode -1)
    (show-paren-mode 1))
  (message "show-paren-style: %s" show-paren-style))

(global-set-key (kbd "C-c C-v") 'my/toggle-paren-style)

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package projectile
  :ensure t
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))

;;------------------------------------------------------------------------------
;; Org Mode
;;------------------------------------------------------------------------------
(use-package org
  :ensure t
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-ellipsis " ▼"))

;;------------------------------------------------------------------------------
;; Prog — Программирование
;;------------------------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :commands lsp lsp-deferred
  :hook (python-mode . lsp-deferred))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-capf))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package pyvenv
  :ensure t)

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package elpy
  :ensure t
  :init (elpy-enable)
  :config (setq elpy-rpc-python-command "python3"))

(use-package yaml-mode)
(use-package json-mode)
(use-package dockerfile-mode)

;;------------------------------------------------------------------------------
;; Extras — Дополнительно
;;------------------------------------------------------------------------------
(use-package counsel
  :ensure t
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        enable-recursive-minibuffers t))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 7)
  (auto-package-update-maybe))

;; Show Colors Preview
(defun my/show-colors-with-preview ()
  "Показать все цвета с предпросмотром, hex-кодом и интерактивными действиями."
  (interactive)
  (let ((buffer (get-buffer-create "*Colors Preview*")))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (color (sort (delete-dups (defined-colors)) 'string<))
        (let* ((hex (color-name-to-rgb color))
               (hex-str (if hex
                            (apply (lambda (r g b)
                                     (format "#%02x%02x%02x"
                                             (round (* r 255))
                                             (round (* g 255))
                                             (round (* b 255))))
                                   hex)
                          "N/A"))
               (text (format "%-30s %s" color hex-str)))

          ;; Вставить квадрат цвета
          (insert (propertize "    " 'face `(:background ,color)))
          (insert " ")

          ;; Вставить имя цвета с подсказкой и действием
          (insert-text-button text
                              'action (lambda (btn)
                                        (let ((col (button-get btn 'color)))
                                          (set-background-color col)
                                          (message "Background set to: %s (%s)" col (or (color-name-to-rgb col) "N/A"))))
                              'help-echo (lambda (window object position)
                                           (format "Color: %s (%s)\n\nClick to set as background." color hex-str))
                              'follow-link t
                              'face `(:foreground ,color)
                              'color color)

          (insert "\n")))
      (setq cursor-type nil) ; убрать курсор в буфере
      (setq header-line-format " *Colors Preview* — Нажмите на цвет, чтобы установить фон. Наведите — чтобы увидеть hex-код."))
    (pop-to-buffer buffer)
    (view-mode 1)
    (message "Наведите курсор на цвет для подсказки. Клик — установить фон.")))

;;------------------------------------------------------------------------------
;; Завершение
;;------------------------------------------------------------------------------
;; Восстанавливаем GC
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 16 1000 1000))))

(message "init.el загружен успешно!")
;;; init.el ends here
