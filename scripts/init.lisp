;;;
;;; init.lisp
;;;

;; The game runs this script
;; upon startup. Therefore,
;; the engine will not allow
;; this script to be edited or
;; to run custom code. Really,
;; it's for your own good. If
;; you were allowed to run
;; your own custom code during
;; boot, and you crashed the
;; boot process, your game
;; would be permanently
;; bricked, unless you had
;; a cartridge flasher to
;; manually erase sram.

(eval-other-file "/scripts/stdlib.lisp")

(def language 'english)
