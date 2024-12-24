;; -*- lexical-binding: t; -*-

(use-package org
  :ensure t
  :bind (:map global-map
              ("C-c l" . #'org-store-link)
              ("C-c a" . #'org-agenda)
              ("C-c c" . #'org-capture))
  :config
  (setf org-startup-with-inline-images t
        org-ellipsis "..."
        org-hide-leading-stars t
        org-superstar-headline-bullets-list '(?\ )
        org-indent-indentation-per-level 2
        org-indent-mode-turns-on-hiding-stars nil
        org-display-remote-inline-images 'download
        org-image-actual-width nil
        org-return-follows-link t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-superstar-mode 1)
              (org-indent-mode 1))))

(defface org-checkbox-todo
  '((t (:inherit org-checkbox)))
  "")

(defface org-checkbox-done
  '((t (:inherit org-checkbox :foreground "#427b58")))
  "")

(font-lock-add-keywords 'org-mode
                        `((,(rx "[ ]") 0 'org-checkbox-todo prepend)
                          (,(rx "[X]") 0 'org-checkbox-done prepend)))

