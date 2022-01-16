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

(eval-file "/scripts/stdlib.lisp")

(setq language 'english)

;; Let's define some useful
;; builtin functions:

(defn/c cargo-bays
  (let ((rooms (rooms $0)))
    (map
     (lambda
       (cons
        (car (cdr $0))
        (car (cdr (cdr $0)))))
     (filter
      (lambda (equal (car $0) 'cargo-bay))
      rooms))))
