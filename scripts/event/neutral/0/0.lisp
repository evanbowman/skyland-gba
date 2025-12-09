
(eval-file
 (case (faction)
   ('goblin "scripts/event/neutral/0/0_goblin.lisp")
   ('sylph  "scripts/event/neutral/0/0_sylph.lisp")
   (else    (format "scripts/event/neutral/0/0_%.lisp" (choice 2)))))
