;; -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setf files-to-load '(misc theme completion gnus evil lsp org slime web prettify)
      configuration-path (expand-file-name "~/.emacs.d/lisp/"))

(defun load-config (name)
  (load (format "%s%s" configuration-path name)))

(dolist (file files-to-load)
  (load-config file))

(defun replace-defun ()
  "Evalute current Defun, then replace it with it's return value."
  (interactive)
  (let* ((region (bounds-of-thing-at-point 'sexp))
         (string (buffer-substring-no-properties (car region) (cdr region))))
    (cl-case major-mode
      ((lisp-interaction-mode emacs-lisp-mode) (delete-region (car region) (cdr region))
                       (let ((form (car (read-from-string string))))
                         (insert (format "%s" (eval form)))))
      (lisp-mode (delete-region (car region) (cdr region))
                 (slime-eval-print string)))))

(keymap-global-set "H-r" #'replace-defun)

(use-package projectile
  :ensure t
  :config
  (setf projectile-project-search-path (list "~/projects/")
        projectile-generic-command "fd -L . -0 --type f --color=never --strip-cwd-prefix"
        projectile-keymap (make-sparse-keymap))
  (cl-pushnew "~/dotfiles/" projectile-known-projects :test #'string=)
  (projectile-load-known-projects)
  (projectile-discover-projects-in-search-path)
  (keymap-global-set "H-p" projectile-keymap)
  (keymap-set projectile-keymap "H-a" #'projectile-add-known-project)
  (keymap-set projectile-keymap "a"   #'projectile-add-known-project)
  (keymap-set projectile-keymap "H-s" #'projectile-switch-project)
  (keymap-set projectile-keymap "s"   #'projectile-switch-project)
  (keymap-set projectile-keymap "H-f" #'projectile-find-file)
  (keymap-set projectile-keymap "f"   #'projectile-find-file)) 
