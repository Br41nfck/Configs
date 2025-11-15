(global-set-key (kbd "<f9>") 'compile)
;(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs")))

(defun my/open-org-dir ()
  "Открыть папку с моими org-файлами в Dired."
  (interactive)
  (dired "~/.config/emacs/ORG/"))

(global-set-key (kbd "C-c o") #'my/open-org-dir)

(global-set-key (kbd "C-c g") 'replace-regexp)
(global-set-key (kbd "C-c p") 'package-list-packages)
(global-set-key (kbd "<f12>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f8>") 'comment-region)
;; Work with Buffers
(global-set-key (kbd "<f4>")    'eval-buffer)
; Incompatible with EWW-Search function
;(global-set-key (kbd "<f5>")    'save-buffer)
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
;; Swiper
(global-set-key (kbd "C-s") 'swiper)
(provide 'my-keybinds)
