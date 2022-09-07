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

(defn/c clamp
  (cond
   ((< $0 $1) $1)
   ((> $0 $2) $2)
   (true $0)))

(defn/c procgen
  (opponent-generate
   (cond
    ((equal (zone) 0)
     (clamp (- (length enemies-seen) 1) 0 3))
    ((equal (zone) 1) 5)
    ((equal (zone) 2) 12)
    (true 16))))

(defn/c zone
  (car (wg-pos)))

;; Choose a random element of a list.
(defn/c sample
  (get $0 (choice (length $0))))
