;; -*- lexical-binding: t; -*-

(setf custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(global-display-line-numbers-mode 1)
(fringe-mode 0)

(set-face-attribute 'default nil :family "0xProto Nerd Font" :height 120)
(add-hook 'prog-mode-hook (lambda () (setf tab-width 4 indent-tabs-mode nil)))

(setf display-line-numbers-type 'relative
      frame-resize-pixelwise t
      warning-minimum-level :error
      inhibit-startup-screen t
      backup-directory-alist `(("." . ,temporary-file-directory))
      browse-url-browser-function '(("hyperspec" . eww-browse-url)
				                    ("." . browse-url-default-browser))
      treesit-font-lock-level 4)

(async-bytecomp-package-mode 1)

(add-hook 'c-mode-hook (lambda ()
			             (c-toggle-electric-state 1)
			             (c-toggle-comment-style -1)
			             (c-toggle-cpp-indent-to-body 1)
			             (c-set-style "stroustrup")))

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode 1)))

(dotimes (tab 10)
  (keymap-global-set (format "C-%d" tab) (lambda ()
                                           (interactive)
                                           (tab-bar-select-tab tab))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (setf mc/always-run-for-all t)
  (add-hook 'prog-mode-hook (lambda (&rest args) (multiple-cursors-mode 1))))

(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-mpris-enable)
  (setf emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-browser-thumbnail-small-size 128
        emms-browser-thumbnail-medium-size 256
        emms-browser-thumbnail-large-size 512
        emms-source-playlist-default-format 'm3u
        emms-source-file-default-directory "~/Music/"
        emms-player-list (list emms-player-mpv)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package docker
  :ensure t
  :config
  (keymap-global-set "C-c d" #'docker))

(keymap-global-set "C-x C-d" #'dired)
(keymap-global-set "C-x d" #'list-directory)

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
(keymap-global-set "s-d" #'ispell-change-dictionary)
(setf dired-dwim-target #'dired-dwim-target-next-visible
      dired-create-destination-dirs 'always
      dired-listing-switches "-alh" 
      dired-mouse-drag-files t
      dired-auto-revert-buffer #'dired-directory-changed-p)
