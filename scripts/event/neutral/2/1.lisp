
(cond
 ((equal (difficulty) 0)
  ;; use the easier variant
  (eval-file "/scripts/event/neutral/2/1_alt0.lisp"))
 ((equal (difficulty) 1)
  ;; randomly use a harder level
  (eval-file (format "/scripts/event/neutral/2/1_alt%.lisp" (choice 2))))
 (true
  ;; harder variant only
  (eval-file "/scripts/event/neutral/2/1_alt1.lisp")))
