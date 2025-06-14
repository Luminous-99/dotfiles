(in-package :stumpwm)

(setf *debug-level* 6)

(load-module "mem")
(load-module "ttf-fonts")
(load-module "cpu")
(load-module "battery-portable") 
(load-module "wifi")
(load-module "swm-gaps")

(asdf:load-asd (concatenate 'string  (uiop:getenv "HOME") "/.stumpwm.d/stump.asd"))
(asdf:load-system "stumpwm-configuration")
