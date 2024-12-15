
(cond
  ((equal (faction) 'goblin)
   (eval-file "/scripts/event/neutral/0/5_goblin.lisp"))
  (true
   (eval-file "/scripts/event/neutral/0/5_human.lisp")))
