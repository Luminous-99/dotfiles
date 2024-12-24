;; -*- lexical-binding: t; -*-

(use-package nerd-icons
  :ensure t)

(defun prettify ()
  (setf prettify-symbols-alist '(("lambda" . ?λ) ("[X]" . ?󰱒) ("[ ]" . ?󰄱) ("[-]" . ?󰡖)))
  (prettify-symbols-mode 1))

(add-hook 'prog-mode-hook #'prettify)
(add-hook 'org-mode-hook #'prettify)

(load-config "ligatures")
