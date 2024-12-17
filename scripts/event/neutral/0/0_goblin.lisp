;;;
;;; neutral/0/0_goblin.lisp
;;;


(dialog
 "In the distance, you see an island inhabited by a lone castaway...")


(opponent-init 6 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 12)
   (hull 5 14)))


(secret
 5 14
 "Days stranded: |||| |||| |||| |||| ||||...")


(chr-new (opponent) 1 14 'neutral '((race . 1)))


(defn on-converge ()
  (dialog
   "<c:castaway:38>Yargh!! About time ssomeone showed up! <B:0> I don't even know how long I've been ssstranded here. I've been kicked out of the goblin horde, how could they do this!?")

  (setq on-dialog-closed
        (lambda ()
            (dialog "He seems decent, invite him aboard?")

          (dialog-await-binary-q-w/lore "welcome aboard!" "not today"
                                        '(("what happened?" .
                                           "<c:castaway:38> We were given ordersss to raid a human sship. <B:0> Nothing special. For sssome reason I hesitated, and was thrown overboard. <B:0> I can make ammends, I'll show you, I'm still vicious, I sswear!")))

          (setq on-dialog-closed '())))

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (run-util-script
   "find-crew-slot"
   "<c:castaway:38>Hold on, don't leave me here! I know your casstle's full, but I can help make some ssspace!"
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (adventure-log-add 7 '())
     (chr-del (opponent) 1 14)
     (chr-new (player) x y 'neutral '((race . 1) (icon . 38)))
     (dialog "<c:castaway:38> Thanks for ressscuing me! I'll try to help out however I can! <B:0> Whatt? You've been banisshed too? <B:0> Ha! HAHAHA! SSome sorry lot of goblins we are!")
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (if (or (chance 2) (< (coins) 300))
           (dialog "The castaway joined your crew!")
           (progn
             (coins-set (- (coins) 300))
             (dialog "The castaway joined your crew. Starving, he ate 300@ of your food supplies!")))
       (exit)))))

(setq on-dialog-declined exit)
