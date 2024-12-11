
(cond
 ((equal (difficulty) 0)
  ;; use the easier variant
  (load-level "/scripts/event/neutral/2/1_alt0.lisp"))
 ((equal (difficulty) 1)
  ;; randomly use a harder level
  (if (equal (choice 3) 0)
      (load-level "/scripts/event/neutral/2/1_alt1.lisp")
    (load-level "/scripts/event/neutral/2/1_alt0.lisp")))
 (true
  ;; harder variant only
  (load-level "/scripts/event/neutral/2/1_alt1.lisp")))
