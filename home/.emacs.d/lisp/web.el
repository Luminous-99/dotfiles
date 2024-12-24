;; -*- lexical-binding: t; -*-

(use-package tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (tsx-ts-mode . tide-hl-identifier-mode)
         (typescript-ts-mode . tide-hl-identifier-mode)))

(defun init-web-mode ()
  (setf web-mode-markup-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-css-indent-offset 4)
  (lsp-deferred))

(defun setup-tide-mode ()
  (interactive)
  (indent-tabs-mode nil)
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (company-mode 1))

(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook #'init-web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

(use-package php-mode
  :ensure t
  :config
  (defun init-php-mode ()
    (lsp-deferred))
  (add-hook 'php-mode-hook #'lsp-deferred))
