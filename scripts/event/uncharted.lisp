;;;
;;; uncharted.lisp
;;;
;;; Some unexpected, randomly occuring, events.
;;;


(eval-file "scripts/event/hostile.lisp")


(let ((prob (get '(30 15 8) (difficulty)))          ; chance of surprise event occuring
      (lvs (difference surprises-seen (range 2))))  ; list of avail surprise events

  (if (and lvs
           (not (has-dialog?))
           (equal (choice prob) 0))
      (let ((lv (sample lvs)))
        (setq surprises-seen (cons lv surprises-seen))
        (procgen)
        (eval-file (format "scripts/event/surprise/%.lisp" lv)))))
