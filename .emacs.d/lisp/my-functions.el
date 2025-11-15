(defun my/org-insert-src-block (src-code-type)
  "Вставить блок кода в org-mode."
  (interactive
   (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "C++" "latex" "org")))
     (list (ivy-completing-read "Source code type: " src-code-types))))
  (insert (format "#+BEGIN_SRC %s\n" src-code-type))
  (insert "#+END_SRC\n"))

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

(provide 'my-functions)

