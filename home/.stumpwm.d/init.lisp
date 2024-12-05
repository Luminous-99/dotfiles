;; (eval
;;  `(asdf:defsystem "stumpwm-configuration"
;;     :pathname ,*stumpwm-path*
;;     :depends-on (:uiop :clx-truetype :stumpwm :swank)
;;     :components
;;     ((:file "util")
;;      (:file "audio")
;;      (:file "keys"))))

;; (asdf:load-system "stumpwm-configuration")

(ql:quickload :swank)
(ql:quickload :stumpwm)
(ql:quickload :clx-truetype)
(in-package :stumpwm)

(load-module "swm-gaps")

(swm-gaps:toggle-gaps-off)

(defun toggle-gaps ()
  (if (null (cdr (group-frames (current-group))))
      (swm-gaps:toggle-gaps-off)
      (swm-gaps:toggle-gaps-on)))

(defun window-exists? (class)
  (let ((windows (group-windows (current-group))))
    (loop for window in windows
          when (classed-p window class)
            return t)))

(defun run-program (command &optional (class nil))
  (unless (window-exists? class)
    (uiop:launch-program command)))

(defun run-programs (programs)
  (dolist (program programs)
    (destructuring-bind (name . args) program
      (run-program (concatenate 'string name " " args) name))))

(defun current-track ()
  (concatenate 'string "Playing: "
               (string-trim '(#\Newline)
                            (run-shell-command
                             "playerctl metadata --format \"{{trunc(artist,14)}} - {{trunc(title,15)}}\"" t))))

(defcommand xmod-prefix () ()
  (run-shell-command "xmodmap -e \"clear mod4\"" t)
  (run-shell-command "xmodmap -e \"keycode 133 = F20\"" t)
  (set-prefix-key (kbd "F20")))

(xmod-prefix)

(defmacro define-keys (map &body keys)
  `(dolist (key ',keys)
     (undefine-key ,map (kbd (car key)))
     (define-key ,map (kbd (car key))
       (cdr key))))

(defmacro undefine-keys (map &body keys)
  `(dolist (key ',keys)
     (undefine-key ,map (kbd key))))

(defparameter *swank-running* nil)
(defcommand swank () ()
  (swank:list-threads)
  (unless *swank-running*
    (swank:create-server :port 4005
			             :style swank:*communication-style*
			             :dont-close t)
    (echo-string (current-screen) 
                 "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm).")
    (setf *swank-running* t)))

(swank)

(defun set-window-property (window property value)
  (xlib:change-property (window-xwin window) property (list value) :integer 32))

(defun set-floating (window floating)
  (if floating
      (progn
        (float-window window (current-group))
        (set-window-property window :_STUMPWM_FLOATING 1))
      (progn
        (unfloat-window window (current-group))
        (set-window-property window :_STUMPWM_FLOATING 0))))


;; General audio
(defcommand volume-up () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Up"))
(defcommand volume-down () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Down"))
(defcommand volume-mute () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Mute"))
(defcommand brightness-up () ()
  (run-shell-command "~/.stumpwm.d/scripts/brightness_notify.sh Up"))
(defcommand brightness-down () ()
  (run-shell-command "~/.stumpwm.d/scripts/brightness_notify.sh Down"))

(defmacro when-return (expr)
  (with-gensyms (val)
    `(let ((,val ,expr))
	   (if ,val ,val nil))))

;; Sort windows on destruction
(defun sort-current-group (window)
  (declare (ignorable window))
  (let* ((windows (list-windows (current-group)))
         (windows (sort windows (lambda (x y)
                                  (when y 
                                    (< (window-number x)
                                       (window-number y)))))))
    (do ((rest windows (cdr rest))) ((null (cdr rest)) rest)
      (let ((num1 (window-number (car rest))) 
            (num2 (window-number (cadr rest)))
            (win (cadr rest)))
        (when (and num2 (> num2 (1+ num1))) 
          (setf (window-number win) (1+ num1)))))))

(setf *destroy-window-hook* (list #'sort-current-group))

(defparameter *player* "spotify")

(defcommand set-player (player) ((:string "Enter player: "))
  (unless (string= player "")
    (setf *player* player)))

(defcommand toggle-track () ()
  (run-program (format nil ".stumpwm.d/scripts/player_notify.sh ~a" *player*))
  (run-program (format nil "playerctl --player=~a play-pause" *player*)))

(defcommand next-track () ()
  (run-program (format nil "playerctl --player=~a next" *player*)))

(defcommand previous-track () ()
  (run-program (format nil "playerctl --player=~a previous" *player*)))

(defparameter *resizing-mode* nil)
(setf *resize-increment* 20)

(defun float-move-resize (window &key x y width height)
  (float-window-move-resize window
                            :x (if x x (window-x window))
                            :y (if y y (window-y window))
                            :width (if width width (window-width window))
                            :height (if height height (window-height window))))

(defun float-resize (window dir)
  (with-slots (width height) window
    (case dir
      (:up (float-move-resize window :height (- height *resize-increment*)))
      (:left (float-move-resize window :width (- width *resize-increment*)))
      (:right (float-move-resize window :width (+ width *resize-increment*)))
      (:down (float-move-resize window :height (+ height *resize-increment*))))))

(defun float-move (window dir)
  (with-slots (x y) window
    (case dir
      (:up (float-move-resize window :y (- y *resize-increment*)))
      (:left (float-move-resize window :x (- x *resize-increment*)))
      (:right (float-move-resize window :x (+ x *resize-increment*)))
      (:down (float-move-resize window :y (+ y *resize-increment*))))))

(defcommand move (dir) ((:direction "Direction: "))
  (let ((window (current-window)))
    (if (float-window-p window)
        (if *resizing-mode*
            (float-resize window dir)
            (float-move window dir))
        (move-focus dir))))

(defcommand iresize-float () ()
  (setf *resizing-mode* (not *resizing-mode*))
  (if *resizing-mode*
      (define-keys *top-map*
        ("Left" . "move left")
        ("Right" . "move right")
        ("Up" . "move up")
        ("Down" . "move down")
        ("RET" . "iresize-float"))
      (undefine-keys *top-map* "Left" "Right" "Up" "Down" "RET")))

(defcommand close-window-and-frame () ()
  (unless (only-one-frame-p)
    (delete-window)
    (remove-split))
  (toggle-gaps))

(defcommand toggle-maximize () ()
  (if (window-max (current-window))
      (unfloat-this)
      (float-this)))

(defcommand terminate-this () ()
  (let ((window (current-window)))
    (kill-window window)
    (toggle-gaps)))

(defcommand toggle-float () ()
  (let ((window (current-window)))
    (if (float-window-p window)
        (set-floating window nil)
        (set-floating window t))))

;; Audio 
(define-keys *top-map*
  ("XF86AudioRaiseVolume" . "volume-up")
  ("XF86AudioLowerVolume" . "volume-down")
  ("XF86AudioMute" . "volume-mute"))

;; Music
(define-keys *root-map*
  ("M-P" . "set-player")
  ("M-t" . "toggle-track")
  ("M-n" . "next-track")
  ("M-p". "previous-track"))

;; Brightness
(define-keys *top-map*
  ("XF86MonBrightnessUp" . "brightness-up")
  ("XF86MonBrightnessDown" . "brightness-down"))
(define-keys *root-map*
  ("B". "brightness-up")
  ("b" . "brightness-down"))

;; Programs
(defcommand screenshot () ()
  (run-shell-command "flameshot gui"))

(define-keys *root-map*
  ("d" . "exec dmenu_run -fn 0xProto -nb \"#f2e5bc\" -nf \"#3c3836\"")
  ("D" . "exec rofi -show drun")
  ("M-f" . "screenshot")
  ("M-d" . "exec rofi -show recursivebrowser"))

(defcommand delete-split () ()
  (remove-split)
  (toggle-gaps))

(defcommand split-horizontal () ()
  (hsplit)
  (toggle-gaps))

(defcommand split-vertical () ()
  (vsplit)
  (toggle-gaps))

;; Windows
(define-keys *root-map*
  ("f" . "toggle-float")
  ("F" . "toggle-always-on-top")
  ("R" . "iresize-float")
  ("x" . "delete-split")
  ("X" . "close-window-and-frame")
  ("s" . "split-vertical")
  ("S" . "split-horizontal")
  ("k" . "terminate-this")
  ("C-k" . "terminate-this")
  ("K" . "terminate-this")
  ("Up" . "move up")
  ("Left" . "move left")
  ("Right" . "move right")
  ("Down" . "move down")
  ("M-Up" . "exchange-direction up")
  ("M-Left" . "exchange-direction left")
  ("M-Right" . "exchange-direction right")
  ("M-Down" . "exchange-direction down"))

;; Groups
(dotimes (i 9)
  (define-key *root-map* (kbd (format nil "M-~a" (1+ i)))
    (format nil "gmove ~a" (1+ i))))


(dotimes (i 7)
  (gnew (format nil "Group ~a" (+ i 2))))
(gnew-dynamic "Group 9")

(defparameter *startup-programs* '(("kdeconnectd" . "")
                                   ("emacsclient" . "--alternate-editor= -c")
                                   ("firefox" . "")
                                   ("vesktop" . "")
                                   ("steam" . "-silent")))

(run-programs '(("picom" . "-b --vsync -f")
                ("dunst" . "-conf ~/.config/dunst/dunstrc")
                ("feh" . "--no-fehbg --bg-scale ~/dotfiles/Background/The_Garden_of_earthly_delights_Reduced.jpg")))

(defcommand auto-start () ()
  (run-programs *startup-programs*))

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
      *mode-line-border-width* 0
      swm-gaps:*outer-gaps-size* 5
      swm-gaps:*inner-gaps-size* 5)

(set-bg-color "#f2e5bc")
(set-fg-color "#3c3836")

;; xfontsel example just in case.
;; -------------------------
;; -misc-0xproto nerd font-medium-r-normal--12-120-100-100-p-0-iso8859-16
;; (set-font "-misc-0xproto nerd font-medium-r-normal--17-110-110-110-p-0-iso8859-16")
;; -------------------------
(load-module "ttf-fonts")
(xft:cache-fonts)
(defparameter *font*
  (make-instance 'xft:font
                 :family "0xProto Nerd Font"
                 :subfamily "Regular"
                 :size 12))
(set-font *font*)

(load-module "mem")
(load-module "cpu")
(load-module "battery-portable") 
(load-module "wifi")
(setf *window-format* "%m%n%s%c ")
(setf *group-format* "[%n]")
(setf *screen-mode-line-format*
      (list "%g %W ^> %C | %M | Battery: %B | %d"))
(setf *time-modeline-string* "%a %b %e %k:%M")
(setf *mode-line-timeout* 2)
(enable-mode-line (current-screen) (current-head) t)
(xmod-prefix)
(gselect "1")
