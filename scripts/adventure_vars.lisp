;;;
;;; adventure_vars.lisp
;;;
;;; Some variables used to keep state in adventure mode.
;;;
;;; You may set boolean variables, associated with symbols, by using
;;; adv-var-load and adv-var-store.
;;;


(setq adv-var-list '(mercenary-event
                     sylph-shop-intro))

(when (> (length adv-var-list) 31)
  (fatal "Too many adventure vars! Cannot fit in integer bitfield adv-var-set"))

(setq adv-var-set 0)

(configure-vars
 '((1 . "powerdown_allowed")
   (0 . "rewind_disabled")))

(when (equal (difficulty) difficulty-beginner)
  (setvar "cold_boot_penalty_ms" 0))
