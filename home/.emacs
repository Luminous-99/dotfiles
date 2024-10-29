(setf lexical-binding t)
(setf custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq calendar-date-style 'european)
(setq user-mail-address (getenv "MAIL_ADDRESS")
      user-full-name "luminous99"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")

(use-package gnus
  :ensure t
  :config
  (setq gnus-article-sort-functions
	'(gnus-article-sort-by-date
	  gnus-article-sort-by-number gnus-article-sort-by-author
	  gnus-article-sort-by-subject gnus-article-sort-by-score)
	gnus-select-method '(nnimap "gmail"
				    (nnimap-address "imap.gmail.com")
				    (nnimap-server-port 993)
				    (nnimap-stream ssl)))
  (gnus-demon-add-handler 'gnus-demon-scan-news 10 10)
  (gnus-demon-init))

(setf warning-minimum-level :error
      inhibit-startup-screen t
      backup-directory-alist `(("." . ,temporary-file-directory))
      browse-url-browser-function
      '(("hyperspec" . eww-browse-url)
	("." . browse-url-default-browser)))

(defmacro install-packages (&rest packages)
  `(dolist (package ',packages)
     (package-install package)))

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

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'lisp-mode-hook (lambda (&rest args) (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda (&rest args) (paredit-mode 1)))

(set-face-attribute 'default nil :family "0xProto" :height 120)
(setf prettify-symbols-alist '(("lambda" . 955)))
(add-hook 'lisp-mode-hook   (lambda (&rest args) (prettify-symbols-mode 1)))
(add-hook 'scheme-mode-hook (lambda (&rest args) (prettify-symbols-mode 1)))
(add-hook 'racket-mode-hook (lambda (&rest args) (prettify-symbols-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda (&rest args) (prettify-symbols-mode 1)))

(ligature-set-ligatures t (mapcar #'(lambda (x) (if (symbolp x) (symbol-name x) x))
				  `(-> <- => =>> >=> =>=
				       =<< =<= <=< <=> >> >>>
				       << <<< <> <|> == ===
				       .= := "#=" != !== =!=
				       =:= :: ::: :<: :>: ||
				       |> ||> |||> <| <|| <|||
				       ** *** <* <*> *> <+
				       <+> +> <$ <$> $> &&
				       ?? %% "[|" "|]" // ///)))

(global-ligature-mode 1)
(load-theme 'gruvbox-light-soft t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(global-display-line-numbers-mode 1)
(setf display-line-numbers-type 'relative
      frame-resize-pixelwise t)
(fringe-mode 0)

(dotimes (tab 10)
  (global-set-key (kbd (format "C-%d" tab))
		  `(lambda () (interactive) (tab-bar-select-tab ,tab))))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (company-quickhelp-mode 1))

(use-package slime
  :ensure t
  :config
  (setf inferior-lisp-program "sbcl")
  (keymap-set slime-mode-map "C-c c" #'slime-export-class)
  (keymap-set slime-mode-map "C-c s" #'slime-export-structure)
  (slime-setup '(slime-fancy slime-banner slime-company)))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (add-to-list 'company-backends 'company-slime)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(setf treesit-font-lock-level 4)
(use-package tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
	 (tsx-ts-mode . tide-setup)
	 (tsx-ts-mode . tide-hl-identifier-mode)
	 (typescript-ts-mode . tide-hl-identifier-mode)))

(defun init-web-mode ()
  (setf web-mode-markup-indent-offset 4)
  (setf web-mode-code-indent-offset 4)
  (setf web-mode-css-indent-offset 4)
  (lsp-deferred))

(defun setup-tide-mode ()
  (interactive)
  (indent-tabs-mode nil)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook #'init-web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode)))))

(use-package lsp-mode
  :ensure t
  :config
  (setf lsp-ui-doc-delay 0.5
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-show-with-cursor t)
  (defun toggle-hover ()
    (interactive)
    (setf lsp-ui-doc-show-with-cursor (not lsp-ui-doc-show-with-cursor))
    (lsp-ui-doc-hide))
  (keymap-set lsp-mode-map "C-M-k" #'toggle-hover))

(use-package php-mode
  :ensure t
  :config
  (defun init-php-mode ()
    (lsp-deferred))
  (add-hook 'php-mode-hook #'init-php-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (setf mc/always-run-for-all t)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (add-hook 'prog-mode-hook (lambda (&rest args) (multiple-cursors-mode 1))))

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
  :config
  (evil-set-undo-system 'undo-tree)
  (keymap-unset global-map "C-z")
  (define-key evil-normal-state-map (kbd "C-z") (make-sparse-keymap)) 
  (define-key evil-visual-state-map (kbd "C-z") (make-sparse-keymap)) 
  (keymap-set evil-visual-state-map "C-z C-s" #'shell-command-on-region)
  (keymap-set evil-normal-state-map "C-z C-f" 'ffap)
  (keymap-set evil-normal-state-map "C-S-d"
	      (lambda () (interactive) (evil-scroll-up 0)   (evil-scroll-line-to-center nil)))
  (keymap-set evil-normal-state-map "C-d"
	      (lambda () (interactive) (evil-scroll-down 0) (evil-scroll-line-to-center nil)))
  (keymap-set evil-visual-state-map "C-z C-f" 'ffap)
  :init (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
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

(use-package nerd-icons
  :ensure t)
