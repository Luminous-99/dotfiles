(defpackage misc
  (:use :cl :stumpwm :clx-truetype)
  (:export
   #:define-keys
   #:undefine-keys
   #:brightness-up
   #:brightness-down
   #:window-exists?
   #:run-program
   #:run-programs
   #:run-formatted
   #:x-setup
   #:*font*
   #:*foreground-color*
   #:*background-color*
   #:rgb-string->integer
   #:integer->rgb-string
   #:run-swank
   #:auto-start
   #:screenshot))

(in-package :misc)

(defvar *swank-running* nil)
(defcommand run-swank () ()
  (unless *swank-running*
    (swank:create-server :port 4005
                   :style swank:*communication-style*
                   :dont-close t)
    (echo-string (current-screen) "Starting swank. Do \"M-x slime-connect.\"")
    (setf *swank-running* t)))

(run-swank)

;; xfontsel example just in case.
;; -------------------------
;; -misc-0xproto nerd font-medium-r-normal--12-120-100-100-p-0-iso8859-16
;; (set-font "-misc-0xproto nerd font-medium-r-normal--17-110-110-110-p-0-iso8859-16")
;; -------------------------
(load-module "ttf-fonts")
(defparameter *font*
  (make-instance 'xft:font
                 :family "0xProto Nerd Font"
                 :subfamily "Regular"
                 :size 12))

(set-font *font*)

(defun rgb-string->integer (hex)
  (parse-integer (subseq hex 1) :radix 16))

(defun integer->rgb-string (integer)
  (format nil "#~x" integer))

(defparameter *foreground-color* "#333333")
(defparameter *background-color* "#f2f2f2")
(set-bg-color *background-color*)
(set-fg-color *foreground-color*)
(set-float-focus-color "#555555")
(set-float-unfocus-color *background-color*)

(defcommand x-setup () ()
  "Set up SUPER and HYPER"
  (run-shell-command "xmodmap -e \"clear mod4\"" t)
  ;; change Left Windows key to F20 key
  (run-shell-command "xmodmap -e \"keycode 133 = F20\"" t)
  ;; change Menu key to Hyper key
  (run-shell-command "xmodmap -e \"keycode 135 = Hyper_R\"" t)
  (run-shell-command "xsetroot -cursor_name left_ptr" t)
  (set-prefix-key (kbd "F20")))

(x-setup)

(defun window-exists? (class)
  (let ((windows (group-windows (current-group))))
    (loop for window in windows
          when (stumpwm::classed-p window class)
            return t)))

(defun run-program (command &optional (class nil))
  (unless (window-exists? class)
    (uiop:launch-program command)))

(defun run-programs (programs)
  (dolist (program programs)
    (destructuring-bind (name . args) program
      (run-program (concatenate 'string name " " args) name))))

(defun run-formatted (fmt &rest args)
  (string-trim '(#\Newline)
               (uiop:run-program (apply #'format nil fmt args)
                                 :output :string
                                 :error-output :string
                                 :ignore-error-status t)))

(defmacro define-keys (map &body keys)
  `(dolist (key (list ,@(mapcar (lambda (key)
                                  (let ((result (car key)))
                                    (if (stringp result)
                                        `(cons (kbd ,result) ,(cdr key))
                                        key)))
                                keys)))
     (undefine-key ,map (car key))
     (define-key ,map (car key) (cdr key))))

(defmacro undefine-keys (map &body keys)
  `(dolist (key (list ,@(mapcar (lambda (key)
                                  (if (stringp key)
                                      `(kbd ,key)
                                      key))
                                keys)))
     (undefine-key ,map key)))

(dolist (kmap `(,@(stumpwm::top-maps) *root-map* stumpwm::*tile-group-root-map* stumpwm::*group-root-map*))
  (undefine-keys (eval kmap)
    "a"
    "C-a"
    (stumpwm::make-key :keysym -1)
    "C-e"
    "C-c"
    "C-m"
    "C-n"
    "C-p"
    "SPC"
    "C-SPC"
    "C-k"
    "K"
    "C-l"
    "C-w"
    "C-RET"))

(dolist (kmap (list stumpwm::*tile-group-root-map* stumpwm::*group-root-map*))
  (undefine-keys kmap
    "Up"
    "Down"
    "Left"
    "Right"
    "o"
    "s"
    "S"
    "X" 
    "M-TAB"
    "M-Up"
    "M-Down"
    "M-Left"
    "M-Right"))

;; Potential simplification
;; (remove-duplicates (stumpwm::kmap-bindings *root-map*) :test #'equalp :key #'stumpwm::binding-command)

;; Brightness
(defcommand brightness-up () ()
  (run-shell-command "~/.stumpwm.d/scripts/brightness_notify.sh Up"))

(defcommand brightness-down () ()
  (run-shell-command "~/.stumpwm.d/scripts/brightness_notify.sh Down"))

(define-keys *root-map*
  ("B". "brightness-up")
  ("b" . "brightness-down"))
(define-keys *top-map*
  ("XF86MonBrightnessUp" . "brightness-up")
  ("XF86MonBrightnessDown" . "brightness-down"))

;; Programs
(defcommand alacritty () ()
  (run-shell-command "alacritty"))

(defcommand dmenu () ()
  (run-shell-command (format nil "dmenu_run -fn 0xProto -nb ~S -nf ~S" *background-color* *foreground-color*)))

(defcommand app-selector () ()
  (run-shell-command  "rofi -show drun"))

(defcommand file-selector () ()
  (run-shell-command  "rofi -show recursivebrowser"))

(define-keys *root-map*
  ("d" . "dmenu")
  ("D" . "app-selector")
  ("M-d" . "file-selector")
  ("c" . "alacritty"))

(defcommand screenshot () ()
  (run-shell-command "flameshot gui"))

(defcommand mail (&optional (to "") (subject "")) ((:string "To: ") (:string "Subject: "))
  (let* ((sexp (format nil "(compose-mail \\\"~a\\\" \\\"~a\\\")" to subject))
         (sexp (format nil "(progn ~a (raise-frame))" sexp))
         (cmd (format nil "emacsclient --eval \"~a\"" sexp)))
    (run-shell-command cmd)))

(define-keys *top-map*
  ("Print" . "screenshot")
  ("XF86Mail" . "mail \"\" \"\"")
  ("C-XF86Mail" . "mail"))

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
