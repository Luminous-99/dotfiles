;; -*- lexical-binding: t; -*-

(use-package slime
  :ensure t
  :bind (:map slime-mode-map
	          ("C-c c" . slime-export-class)
	          ("C-c s" . slime-export-structure))
  :config
  (setf inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-banner slime-company)))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (add-to-list 'company-backends 'company-slime)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))
