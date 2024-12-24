;; -*- lexical-binding: t; -*-

(use-package undo-tree
  :ensure t
  :config
  (setf undo-tree-auto-save-history nil)
  :init
  (global-undo-tree-mode 1))


(setf evil-want-integration t
      evil-want-keybinding nil)

(use-package evil
  :ensure t
  :after undo-tree
  :init (evil-mode 1)
  :config
  (dolist (map (list evil-insert-state-map evil-normal-state-map evil-visual-state-map))
    (keymap-unset map "C-z")
    (keymap-unset map "C-z") 
    (keymap-set map "C-z" (make-sparse-keymap)) 
    (keymap-set map "C-z C-s" #'shell-command-on-region)
    (keymap-set map "C-z C-f" #'ffap))
  (keymap-set evil-normal-state-map "g D" #'lsp-find-declaration)
  (keymap-set evil-normal-state-map "g d" #'lsp-find-definition)
  (keymap-set evil-normal-state-map "g e" #'lsp-treemacs-errors-list)
  (keymap-set evil-normal-state-map "g i" #'lsp-find-implementation)
  (keymap-set evil-normal-state-map "g r" #'lsp-rename)
  (keymap-set evil-normal-state-map "g c" #'lsp-execute-code-action)
  (keymap-set evil-normal-state-map "g R" #'lsp-find-references)
  (evil-set-undo-system 'undo-tree)
  (keymap-set evil-normal-state-map "C-S-d"
              (lambda ()
                (interactive)
                (evil-scroll-up 0)
                (evil-scroll-line-to-center nil)))
  (keymap-set evil-normal-state-map "C-d"
              (lambda ()
                (interactive)
                (evil-scroll-down 0)
                (evil-scroll-line-to-center nil))))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))
