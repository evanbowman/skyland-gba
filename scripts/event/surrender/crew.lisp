;;;
;;; surrender/crew.lisp
;;;

(tr-bind-current)

(find-crew-slot-cb
 (if (equal (faction) 'goblin)
     (tr "<c:Goblin:18>Gahh! <B:0> You are out of ssspace! <B:0> Nice friendsss will not leave usss! I'll build an ledarr!")
     (tr "<c:Goblin:18>Gahh! <B:0> You are out of ssspace! <B:0> Nice humansss will not leave usss! I'll build an ledarr!"))
 'ladder
 (tr "Place block (1x2):")
 (lambda (x y _)
   (chr-new (player)
            x
            y
            'neutral
            (list (cons 'race 1)
                  (cons 'icon (sample '(18 35 36 37 41)))))
   (adventure-log-add 51 '())
   (dialog (tr "One of the goblins joined your crew!"))
   (hostile-pickup-cart 8
                        (tr "While scanning the goblin fortress' computers, you find some fascinating images of the surface world. You record them on a cartridge...")
                        exit)
   (exit 2)))
