
(eval-file
 (cond
   ((equal (faction) 'sylph)
    "scripts/event/neutral/0/0_sylph.lisp")
   ((equal (faction) 'goblin)
    "scripts/event/neutral/0/0_goblin.lisp")
   (true
    (format "scripts/event/neutral/0/0_%.lisp" (choice 2)))))
