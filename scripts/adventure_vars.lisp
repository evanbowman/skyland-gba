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

(defn/c adv-var-mask ((name . symbol))
  (let ((found (find name adv-var-list)))
    (when (nil? found)
      (fatal (string "bad adv var " name)))
    (bit-shift-left 1 found)))

(defn/c adv-var-load ((name . symbol))
  (bit-and adv-var-set (adv-var-mask name)))

(defn/c adv-var-store ((name . symbol) val)
  (let ((mask (adv-var-mask name)))
    ;; clear slot
    (setq adv-var-set (bit-and adv-var-set (bit-not mask)))
    (if val
        (setq adv-var-set (bit-or adv-var-set mask)))))


(if (equal (difficulty) difficulty-beginner)
    (setvar "powerdown_allowed" 1)
    (setvar "powerdown_allowed" 0))

(setvar "rewind_disabled" 0)
