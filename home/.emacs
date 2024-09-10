(setf lexical-binding t)
(setf custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setf warning-minimum-level :error
      inhibit-startup-screen t
      backup-directory-alist `(("." . ,temporary-file-directory)))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(set-face-attribute 'default nil :family "0xProto" :height 120)
(load-theme 'gruvbox-light-soft t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(global-display-line-numbers-mode 1)
(setf display-line-numbers 'relative)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defmacro install-packages (&rest packages)
  `(dolist (package ',packages)
     (package-install package)))

(dotimes (tab 10)
  (global-set-key (kbd (format "C-%d" tab)) `(lambda () (interactive) (tab-bar-select-tab ,tab))))

(use-package undo-tree
  :ensure t
  :config
  (setf undo-tree-auto-save-history nil)
  :init
  (global-undo-tree-mode 1))

(use-package slime
  :ensure t
  :config
  (setf inferior-lisp-program "sbcl")
  (slime-setup '(slime-company)))

(use-package company
  :ensure t
  :init
  (global-company-mode 1)
  (company-quickhelp-mode 1))

(use-package evil
  :ensure t
  :after undo-tree
  :init
  (evil-mode 1)
  :config
  (setf evil-want-integration t
	evil-want-keybinding nil)
  (evil-set-undo-system 'undo-tree)
  (keymap-unset global-map "C-z")
  (define-key evil-normal-state-map (kbd "C-z") (make-sparse-keymap)) 
  (define-key evil-visual-state-map (kbd "C-z") (make-sparse-keymap)) 
  (keymap-set evil-visual-state-map "C-z C-s" #'shell-command-on-region)
  (keymap-set evil-normal-state-map   "C-z C-f" 'ffap)
  (keymap-set evil-normal-state-map   "C-S-d" (lambda () (interactive) (evil-scroll-up 0)   (evil-scroll-line-to-center nil)))
  (keymap-set evil-normal-state-map   "C-d" (lambda () (interactive) (evil-scroll-down 0) (evil-scroll-line-to-center nil)))
  (keymap-set evil-visual-state-map   "C-z C-f" 'ffap))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

(use-package emms
  :ensure t
  :config
  (emms-all)
  (setf emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setf emms-browser-thumbnail-small-size 128)
  (setf emms-browser-thumbnail-medium-size 256)
  (setf emms-browser-thumbnail-large-size 512)
  (setf emms-source-playlist-default-format 'm3u)
  (setf emms-player-list '(emms-player-mpv))
  (emms-add-directory-tree "~/Music"))

(use-package elcord
  :ensure t
  :init
  (elcord-mode 1))
