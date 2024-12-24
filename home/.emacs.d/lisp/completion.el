;; -*- lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :after marginalia
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (company-quickhelp-mode 1))
