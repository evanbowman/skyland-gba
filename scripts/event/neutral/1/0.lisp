
(cond
  ((equal (faction) 'goblin)
   (eval-file "scripts/event/neutral/1/0_goblin.lisp"))
  (true
   (eval-file "scripts/event/neutral/1/0_human.lisp")))
