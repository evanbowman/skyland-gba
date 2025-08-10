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
   "<c:Castaway:38>Yargh!! About time sssomeone showed up! <B:0> I don't even know how long I've been ssstranded here. I've been kicked out of the goblin horde, how could they do this!?")

  (setq on-dialog-closed
        (lambda ()
            (dialog "He seems decent, invite him aboard?")

          (dialog-await-binary-q-w/lore "Welcome aboard!" "Not today."
                                        '(("what happened?" .
                                           "<c:Castaway:38> We were given ordersss to raid a human ssship. <B:0> Nothing ssspecial. For sssome reason I hesssitated, and was thrown overboard. <B:0> I can make amendsss, I'll show you, I'm ssstill vicious, I ssswear!")))

          (setq on-dialog-closed '())))

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (run-util-script
   "find-crew-slot"
   "<c:Castaway:38>Hold on, don't leave me here! I know your cassstle's full, but I can help make some ssspace!"
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (adventure-log-add 7 '())
     (chr-del (opponent) 1 14)
     (chr-new (player) x y 'neutral '((race . 1) (icon . 38)))
     (dialog "<c:Castaway:38> Thanks for ressscuing me! I'll try to help out however I can! <B:0> ... <B:0> What? You've been banissshed too? <B:0> Ha! HAHAHA! Sssome sssorry lot of goblinsss we are!")
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (if (or (chance 2) (< (coins) 300))
           (dialog "The castaway joined your crew!")
           (progn
             (coins-set (- (coins) 300))
             (dialog "The castaway joined your crew. Starving, he ate 300@ of your food supplies!")))
       (exit)))))

(setq on-dialog-declined exit)
