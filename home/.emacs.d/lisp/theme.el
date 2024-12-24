;; -*- lexical-binding: t; -*-

(defun toggle-modus ()
  "Do `modus-themes-toggle' and possibly restart `org-mode'."
  (interactive)
  (modus-themes-toggle)
  (when (eq major-mode 'org-mode)
    (org-mode-restart)))

(use-package modus-themes
  :ensure t
  :config
  (setf modus-themes-common-palette-overrides
        modus-themes-preset-overrides-warmer)
  (setf modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (bold))))
  (setf modus-themes-prompts '(ultrabold)
        modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted))
  (modus-themes-select 'modus-operandi)
  (keymap-global-set "<f5>" #'toggle-modus))
