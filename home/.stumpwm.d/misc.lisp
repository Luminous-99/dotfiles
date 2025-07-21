(defpackage misc
  (:use :cl :stumpwm :clx-truetype :trivial-gray-streams)
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
   #:screenshot
   #:emacs
   #:alacritty
   #:dmenu
   #:app-selector
   #:file-selector
   #:mail
   #:universal-argument
   #:shell-command
   #:run-application)
  (:import-from :symbol-hooks #:define-symbol-hook #:hooked-symbol-p)
  (:import-from :alexandria #:when-let* #:when-let)
  (:shadowing-import-from :symbol-hooks #:setf))

(in-package :misc)

(setf *altgr-offset* 4)
(setf *default-selections* '(:clipboard))
(setf *default-package* (find-package :stumpwm))
(register-altgr-as-modifier)

(defvar *swank-running* nil)
(defcommand run-swank () ()
  (unless *swank-running*
    (swank:create-server :port 4005
                         :style swank:*communication-style*
                         :dont-close t)
    (echo-string (current-screen) "Starting swank. Do \"M-x slime-connect.\"")
    (setf *swank-running* t)))

(run-swank)

(defun rgb-string->integer (hex)
  (parse-integer (subseq hex 1) :radix 16))

(defun integer->rgb-string (integer)
  (format nil "#~x" integer))

(defparameter *foreground-color* "#23139f")
(defparameter *background-color* "#f2f2f2")
(set-fg-color *foreground-color*)
(set-bg-color *background-color*)
(define-symbol-hook *foreground-color*
  (set-fg-color new-value))
(define-symbol-hook *background-color*
  (set-bg-color new-value))
(set-float-focus-color "#555555") 
(set-float-unfocus-color *background-color*)


;; xfontsel example just in case.
;; -------------------------
;; -misc-0xproto nerd font-medium-r-normal--12-120-100-100-p-0-iso8859-16
;; (set-font "-misc-0xproto nerd font-medium-r-normal--17-110-110-110-p-0-iso8859-16")
;; -------------------------

(dolist (path '("/usr/share/fonts/TTF" "/usr/share/fonts/OTF"))
  (pushnew path xft:*font-dirs* :test #'string=)) 
(defparameter *font*
  (make-instance 'xft:font
                 :family "0xProto Nerd Font"
                 :subfamily "Regular"
                 :size 12
                 :antialias t))

(set-font *font*)

(defun run-shell-commands (&rest commands)
  (run-shell-command (format nil "~{~a~^ && ~}" commands) t))

(labels ((sanitize-sexp (sexp)
           (typecase sexp
             (cons (mapcar #'sanitize-sexp sexp))
             (string (format nil "\\\"~A\\\"" sexp))
             (symbol (string-downcase sexp))
             (t sexp))))
  (defmacro with-emacs (&body body)
    "Call Elisp code as directly from common lisp as Sexps."
    (let* ((body (mapcar #'sanitize-sexp body))
           (body (format nil "emacsclient --eval \"(progn ~{~A~^ ~})\"" body)))
      `(values (run-shell-command ,body) ,body))))

(defcommand x-setup () ()
  "Setup X11 related stuff."
  (run-shell-commands
   ;; change Left Windows key to F20 key
   "xmodmap -e \"clear mod4\""
   "xmodmap -e \"add mod4 = Super_R\""
   "xmodmap -e \"keycode 133 = F20\""
   ;; change Menu key to Hyper key
   "xmodmap -e \"keycode 135 = Hyper_R\""
   "xmodmap -e \"add mod3 = Hyper_R\""
   "xsetroot -cursor_name left_ptr")
  (set-prefix-key (kbd "F20")))

(x-setup)

(defun window-exists? (class)
  (let ((windows (group-windows (current-group))))
    (loop for window in windows
          when (string= (window-class window) class)
            return t)))

(defun run-program (command &optional (class nil))
  (if class
      (unless (window-exists? class)
        (uiop:launch-program command))
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
  `(dolist (key (list ,@(mapcar (lambda (key) `(cons (kbd ,(car key)) ,(cdr key))) keys)))
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

(defcommand emacs (&optional (file "") (client t)) ()
  (run-program (format nil "emacs~a ~a" (if client "client --alternate-editor= -c" "") file))
  file)

#+sbcl (pushnew 'emacs cl-user::*ed-functions*)

(defcommand dmenu () ()
  (run-shell-command (format nil "dmenu_run -fn 0xProto -nb ~S -nf ~S" *background-color* *foreground-color*)))

(defcommand app-selector () ()
  (run-shell-command  "rofi -show drun"))

(defcommand file-selector () ()
  (run-shell-command  "rofi -show recursivebrowser"))

(let* ((history (list *shell-program*))
       (history-tail history))
  (defcommand shell-command (&optional cmd) ()
    (let ((cmd (or cmd (completing-read (current-screen) "$ " history)))
          (stumpwm::*input-history* history))
      (setf (cdr history-tail) (list cmd)
            history-tail (cdr history-tail))
      (uiop:launch-program cmd))))

(define-keys *root-map*
  ("d" . "dmenu")
  ("D" . "app-selector")
  ("M-d" . "file-selector")
  ("c" . "alacritty")
  ("t" . "alacritty")
  ("!" . "shell-command"))

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

(run-programs '(("picom" . "-b --vsync -f")
                ("dunst" . "-conf ~/.config/dunst/dunstrc")
                ("feh" . " --no-fehbg --bg-scale ~/dotfiles/Background/Destruction_of_Leviathan.png")
                ("kdeconnectd")))

(defcommand run-application (name) ((:string "Name: "))
  (let ((cmd (format nil "~~/.stumpwm.d/scripts/run-application ~S" name)))
    (run-shell-command cmd)))

(defcommand auto-start (&optional (path #P"~/.stumpwm.d/programs.sexp")) ()
  (with-open-file (file path)
    (let ((programs (read file)))
      (run-programs (getf programs :shell))
      (dolist (name (getf programs :desktop))
        (run-application name)))))

(defun collect-digit-arguments ()
  (prog2 (stumpwm::grab-keyboard (window-xwin (current-window)))
      (loop with key = nil
            while (setf key (read-one-char (current-screen)))
            if (digit-char-p key)
              collect key into keys
            else
              return keys)
    (stumpwm::ungrab-keyboard)))

(defun digit-argument ()
  (let ((digits (coerce (collect-digit-arguments) 'string)))
    (unless (zerop (length digits))
      (parse-integer digits))))

(defun key-argument ()
  (prog2 (stumpwm::grab-keyboard (window-xwin (current-window)))
      (multiple-value-list (stumpwm::read-from-keymap (list *top-map*)))
    (stumpwm::ungrab-keyboard)))

(defcommand universal-argument (&optional number key) ()
  (let ((number (or number (digit-argument)))
        (key (or key (key-argument))))
    (when number
      (handler-case
          (dotimes (i number)
            (let* ((command (car key))
                   (key (caadr key)))
              (if (stringp command)
                  (run-commands command)
                  (stumpwm::send-fake-key (current-window) key))))
        (error (err) (message "~a" err))))))

(define-keys *root-map*
  ("u" . "universal-argument")
  ("M" . "mode-line"))
