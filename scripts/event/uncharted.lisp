;;;
;;; uncharted.lisp
;;;
;;; Some unexpected, randomly occurring, events.
;;;


(eval-file "scripts/event/hostile.lisp")


(let ((prob (get '(26 10 8) (difficulty)))          ; Chance of surprise event occurring.
      (lvs (difference surprises-seen (range 3))))  ; List of available surprise events.

  (if (and lvs
           (not (has-dialog?))
           (equal (choice prob) 0))
      (let ((lv (sample lvs)))
        (setq surprises-seen (cons lv surprises-seen))
        (procgen)
        (eval-file (format "scripts/event/surprise/%.lisp" lv)))))
