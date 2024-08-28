(setf lexical-binding t)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq inferior-lisp-program "sbcl")
(setf warning-minimum-level :error)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(elcord-mode 1)
(setf undo-tree-auto-save-history nil)
(setf backup-directory-alist '(("." . temporary-file-directory)))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (keymap-unset evil-normal-state-map "C-z")
  (keymap-unset evil-insert-state-map "C-z")
  (keymap-set evil-normal-state-map   "C-z C-f" 'ffap)
  (keymap-set evil-insert-state-map   "C-z C-f" 'ffap))

(dotimes (tab 10)
  (global-set-key (kbd (format "C-%d" tab)) `(lambda () (interactive) (tab-bar-select-tab ,tab))))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(with-eval-after-load 'emms
  (emms-all)
  (setf emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setf emms-browser-thumbnail-small-size 128)
  (setf emms-browser-thumbnail-medium-size 256)
  (setf emms-browser-thumbnail-large-size 512)
  (setf emms-source-playlist-default-format 'm3u)
  (setf emms-player-list '(emms-player-vlc emms-player-mpv))
  (emms-add-directory-tree "~/Music"))

(load-theme 'gruvbox-light-soft t)

(setf inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(global-display-line-numbers-mode 1)
