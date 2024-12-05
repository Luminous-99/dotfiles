;; -*- lexical-binding: t; -*-

(setf custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(set-face-attribute 'default nil :family "0xProto Nerd Font" :height 120)

(which-key-mode 1)
(keymap-global-set "C-c d" #'docker)
(keymap-global-set "C-x C-d" #'dired)
(keymap-global-set "C-x d" #'list-directory)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)

(add-hook 'prog-mode-hook (lambda () (setf tab-width 4 indent-tabs-mode nil)))

(use-package org
  :ensure t
  :config
  (set-face-attribute 'org-level-1 nil :height 1.4)
  (set-face-attribute 'org-level-2 nil :height 1.2)
  (set-face-attribute 'org-level-3 nil :height 1.1)
  (set-face-attribute 'org-level-4 nil :height 1.0)
  (dotimes (i 4)
    (set-face-attribute (intern (format "org-level-%d" (+ 5 i))) nil
                        :height 1.0))
  (setq org-startup-with-inline-images t
        org-ellipsis "..."
        org-hide-leading-stars t
        org-superstar-headline-bullets-list '(?\ )
        org-indent-indentation-per-level 2
        org-indent-mode-turns-on-hiding-stars nil
        org-display-remote-inline-images 'download
        org-image-actual-width nil
        org-return-follows-link t)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1) (org-indent-mode 1))))

(setq calendar-date-style 'european)
(setq user-mail-address (getenv "MAIL_ADDRESS")
      user-full-name "luminous99"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")

(defun execute-and-save ()
  "Runs `execute-extended-command' and based on the prefix argument
save it's output from *Messages* to the `kill-ring'."
  (interactive)
  (execute-extended-command current-prefix-arg)
  (when current-prefix-arg
    (with-current-buffer "*Messages*"
      (save-excursion
        (forward-line (1- (car (buffer-line-statistics))))
        (kill-new (thing-at-point 'line))
        (setf (car kill-ring) (string-trim (car kill-ring)))))))

(keymap-global-set "M-x" #'execute-and-save)

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
      browse-url-browser-function '(("hyperspec" . eww-browse-url)
				                    ("." . browse-url-default-browser)))

(add-hook 'c-mode-hook (lambda ()
			             (c-toggle-electric-state 1)
			             (c-toggle-comment-style -1)
			             (c-toggle-cpp-indent-to-body 1)
			             (c-set-style "stroustrup")))

(async-bytecomp-package-mode 1)

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

(defface org-checkbox-todo
  '((t (:inherit org-checkbox)))
  "")

(defface org-checkbox-done
  '((t (:inherit org-checkbox :foreground "#427b58")))
  "")

(font-lock-add-keywords 'org-mode
                        `((,(rx "[ ]") 0 'org-checkbox-todo prepend)
                          (,(rx "[X]") 0 'org-checkbox-done prepend)))


(defun prettify ()
  (setf prettify-symbols-alist '(("lambda" . ?λ) ("[X]" . ?󰱒) ("[ ]" . ?󰄱) ("[-]" . ?󰡖)))
  (prettify-symbols-mode 1))

(add-hook 'lisp-mode-hook   #'prettify )
(add-hook 'scheme-mode-hook #'prettify)
(add-hook 'racket-mode-hook #'prettify )
(add-hook 'emacs-lisp-mode-hook #'prettify)
(add-hook 'org-mode-hook #'prettify)

(ligature-set-ligatures '(prog-mode) (mapcar #'(lambda (x) (if (symbolp x) (symbol-name x) x))
				                             `(-> <- => =>> >=> =>=
				                                  =<< =<= <=< <=> >> >>>
				                                  << <<< <> <|> == ===
				                                  .= := "#=" != !== =!=
				                                  =:= :: ::: :<: :>: ||
				                                  |> ||> |||> <| <|| <|||
				                                  ** *** <* <*> *> <+
				                                  <+> +> <$ <$> $> &&
				                                  ?? %% "[|" "|]" // ///)))

(ligature-set-ligatures '(org-mode) (mapcar #'(lambda (x) (if (symbolp x) (symbol-name x) x))
				                            `(-> <- => =>> >=> =>=
				                                 =<< =<= <=< <=> >> >>>
				                                 << <<< <> <|> == ===
				                                 .= := "#=" != !== =!=
				                                 =:= :: ::: :<: :>: ||
				                                 |> ||> |||> <| <||
                                                 <||| <* <*> *> <+
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

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode 1)))

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
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (setf mc/always-run-for-all t)
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
  :init (evil-mode 1)
  :config
  (dolist (map (list evil-insert-state-map evil-normal-state-map evil-visual-state-map))
    (keymap-unset map "C-z")
    (keymap-unset map "C-z") 
    (keymap-set map "C-z" (make-sparse-keymap)) 
    (keymap-set map "C-z C-s" #'shell-command-on-region)
    (keymap-set map "C-z C-f" #'ffap))
  (evil-set-undo-system 'undo-tree)
  (keymap-set evil-normal-state-map "C-S-d"
              (lambda () (interactive) (evil-scroll-up 0)   (evil-scroll-line-to-center nil)))
  (keymap-set evil-normal-state-map "C-d"
              (lambda () (interactive) (evil-scroll-down 0) (evil-scroll-line-to-center nil))))

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
