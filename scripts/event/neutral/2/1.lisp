;;;
;;; neutral/2/1.lisp
;;;


(cond
 ((equal (difficulty) difficulty-beginner)
  ;; use the easier variant
  (eval-file "/scripts/event/neutral/2/1_alt0.lisp"))
 ((equal (difficulty) difficulty-normal)
  ;; randomly use a harder level
  (if (chance 3)
      (eval-file "/scripts/event/neutral/2/1_alt1.lisp")
    (eval-file "/scripts/event/neutral/2/1_alt0.lisp")))
 (true
  ;; harder variant only
  (eval-file "/scripts/event/neutral/2/1_alt1.lisp")))
