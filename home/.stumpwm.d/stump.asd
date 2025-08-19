(asdf:defsystem "stumpwm-configuration"
  :author "luminous99"
  :pathname #.(concatenate 'string (uiop:getenv "HOME") "/.stumpwm.d/")
  :serial t
  :depends-on (:swank :clx-truetype :alexandria :uiop)
  :components ((:file "symbol-hooks")
               (:file "misc")
               (:file "contrib")
               (:file "windows")
               (:file "audio")
               (:file "mode-line")
               (:file "window-decorator")
               (:file "input")))
