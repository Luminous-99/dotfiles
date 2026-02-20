(asdf:defsystem "stumpwm-configuration"
  :author "luminous99"
  :pathname #.(concatenate 'string (uiop:getenv "HOME") "/.stumpwm.d/")
  :serial t
  :depends-on (:swank :clx-truetype :alexandria :uiop :cl-mpris :cl-freedesktop-notifications)
  :components ((:file "symbol-hooks")
               (:file "misc")
               (:file "contrib")
               (:file "window-decorator")
               (:file "scratchpad")
               (:file "window-preference")
               (:file "windows")
               (:file "audio")
               (:file "mode-line")
               (:file "input")))
