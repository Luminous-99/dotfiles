(in-package #:stumpwm)
(ql:quickload :swank)
(ql:quickload :clx-truetype)

(defcommand swank () ()
  (swank:create-server :port 4005
		       :style swank:*communication-style*
		       :dont-close t)
  (echo-string (current-screen) 
	       "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
(swank)

(uiop:run-program "xmodmap -e \"clear mod4\" && xmodmap -e \"keycode 133 = F20\"")
(set-prefix-key (kbd "F20"))
(uiop:run-program "picom -b --vsync -f")
(defparameter *startup-programs* '(("dunst" . "-conf ~/.config/dunst/dunstrc")
				   ("emacsclient" . "--alternate-editor= -c")
				   ("firefox" . "")
				   ("vesktop" . "")
				   ("steam" . "")))
(dolist (program *startup-programs*)
  (run-or-raise (format nil "~a ~a" (car program) (cdr program))
		`(:class ,(car program))))

(defcommand close-window-and-frame () ()
  (unless (only-one-frame-p)
    (delete-window)
    (remove-split)))

(define-key *root-map* (kbd "d")
  "exec dmenu_run -fn 0xProto -nb \"#f2e5bc\" -nf \"#3c3836\"")

(defparameter *split-dir* nil)

(defun new-window-action (window)
  (if *split-dir*
      (run-commands "vsplit")
      (run-commands "hsplit"))
  (setf *split-dir* (not *split-dir*)))

;; (add-hook *new-window-hook* #'new-window-action)

(define-key *root-map* (kbd "D")
  "exec rofi -show drun")

(undefine-key *root-map* (kbd "M-Up")) 
(define-key *root-map* (kbd "M-Up")
  "exchange-direction up")

(undefine-key *root-map* (kbd "M-Down")) 
(define-key *root-map* (kbd "M-Down")
  "exchange-direction down")

(undefine-key *root-map* (kbd "M-Right")) 
(define-key *root-map* (kbd "M-Right")
  "exchange-direction right")

(undefine-key *root-map* (kbd "M-Left")) 
(define-key *root-map* (kbd "M-Left")
  "exchange-direction left")

(undefine-key *root-map* (kbd "x")) 
(define-key *root-map* (kbd "x")
  "remove-split")
(undefine-key *root-map* (kbd "X")) 
(define-key *root-map* (kbd "X")
  "close-window-and-frame")

(setf *window-border-style* :none
      *new-window-preferred-frame* '(:unfocused)
      *new-frame-action* :empty
      *mouse-focus-policy* :click
      *message-window-gravity* :center
      *input-window-gravity* :center
      *float-window-title-height* 15
      *mode-line-background-color* "#f2e5bc"
      *mode-line-foreground-color* "#3c3836"
      *maxsize-border-width* 0
      *normal-border-width* 0
      *transient-border-width* 0
      *message-window-padding* 5
      *message-window-y-padding* 5
      *mode-line-border-width* 0)

(set-bg-color "#f2e5bc")
(set-fg-color "#3c3836")

;; xfontsel example just in case.
;; -------------------------
;; -misc-0xproto nerd font-medium-r-normal--12-120-100-100-p-0-iso8859-16
;; (set-font "-misc-0xproto nerd font-medium-r-normal--17-110-110-110-p-0-iso8859-16")
;; -------------------------
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font
			 :family "0xProto Nerd Font"
			 :subfamily "Regular"
			 :size 12
			 :spacing :monospace
			 :antialias t))

(load-module "mem")
(load-module "cpu")
(load-module "battery-portable")
(setf *window-format* "%m%n%s%c ")
(setf *screen-mode-line-format*
      (list "[%n] %W^>%C | %M | Battery: %B | %d"))
(setf *time-modeline-string* "%a %b %e %k:%M")
(setf *mode-line-timeout* 1)
(enable-mode-line (current-screen) (current-head) t)

;; (defun set-window-title (window name)
;;   (setf (window-title window) name))

;; (act-on-matching-windows (w) (classed-p w "vesktop")
;; 			 (select-window-by-name (window-title w))
;; 			 (setf (window-title w) "Vesktop"))
