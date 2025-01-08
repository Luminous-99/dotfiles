(asdf:defsystem "stumpwm-configuration"
  :author "luminous99"
  :pathname #.(concatenate 'string (uiop:getenv "HOME") "/.stumpwm.d/")
  :serial t
  :depends-on (:swank :stumpwm :clx-truetype :alexandria :uiop)
  :components ((:file "misc")
               (:file "contrib")
               (:file "windows")
               (:file "audio")
               (:file "mode-line")))
