;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :ensure t
  :config
  (setf lsp-auto-execute-action nil
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor nil)
  (defun toggle-hover ()
    "Toggle hovering documentation."
    (interactive)
    (setf lsp-ui-doc-show-with-cursor (not lsp-ui-doc-show-with-cursor))
    (lsp-ui-doc-hide))
  (keymap-set lsp-mode-map "C-M-k" #'toggle-hover))


(add-hook 'lsp-mode-hook
          (lambda ()
            (keymap-set evil-normal-state-map "g D" #'lsp-find-declaration)
            (keymap-set evil-normal-state-map "g d" #'lsp-find-definition)
            (keymap-set evil-normal-state-map "g e" #'lsp-treemacs-errors-list)
            (keymap-set evil-normal-state-map "g i" #'lsp-find-implementation)
            (keymap-set evil-normal-state-map "g r" #'lsp-rename)
            (keymap-set evil-normal-state-map "g c" #'lsp-execute-code-action)
            (keymap-set evil-normal-state-map "g R" #'lsp-find-references)))
