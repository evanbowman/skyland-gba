;;;
;;; neutral/0/1_0.lisp
;;;


(dialog "A heavily armed marauder fortress approaches. Its captain demands to speak with you.")


(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (cannon 0 14)
   (cannon 0 13)
   (cannon 0 12)
   (missile-silo 3 11)
   (hull 4 12)
   (hull 1 14)
   (hull 2 14)
   (hull 2 13)
   (hull 1 12)
   (hull 2 12)
   (hull 1 13)))

(flag-show (opponent) 1)


(defn on-converge ()
  (dialog "<c:Redbeard:12>Aarrrgh!! You're trespassing in my domain. Gimme 600@ or I'll blast your island to bits!")
  (dialog-opts-reset)
  (dialog-opts-push "Here's 600@…" on-dialog-accepted)

  (dialog-opts-push "You're bluffing!"
                    (lambda ()

                      (defn cb0 ()
                        (emit (opponent) 0 12 (terrain (player)) 0))

                      (defn cb1 ()
                        (emit (opponent) 0 13 (terrain (player)) 0))

                      (defn cb2 ()
                        (emit (opponent) 0 14 (terrain (player)) 0))

                      (defn cb3 ()
                        (dialog "<c:Redbeard:12>Yaargh!! I'm just a simple marauder, trying to earn a decent living here! [via petty extortion, how else?] <B:0> So what's it gonna be? Last chance...")
                        (dialog-await-binary-q "Pay 600@." "Fight back.")
                        (unbind 'cb0 'cb1 'cb2 'cb3))

                      (on-timeout 400 'cb0)
                      (on-timeout 600 'cb1)
                      (on-timeout 800 'cb2)
                      (on-timeout 2000 'cb3)))

  (dialog-opts-push "Never!" on-dialog-declined)
  (setq on-converge nil))


(let ((scr
       (lambda (txt)
         (dialog txt)
         (defn on-dialog-closed ()
           (dialog "<c:goblin:2>Yesss captain!")
           (defn on-dialog-closed ()
             (dialog "(the transmission was cut)")
             (setq on-dialog-closed nil)))
         (opponent-mode 'hostile))))
  (setq on-dialog-accepted
        (lambda ()
          (if (< (coins) 600)
              (progn
                (adventure-log-add 12 '())
                (scr "<c:redbeard:12>That's not enough, load the cannons!!!"))
            (progn
              (adventure-log-add 13 (list 600))
              (coins-add -600)
              (dialog "<c:redbeard:12>Heh. I think you made the smart decision.")
              (exit)))))


  (setq on-dialog-declined
        (lambda ()
          (adventure-log-add 14 '())
          (scr "<c:redbeard:12>Whaatt!! Load the cannons!!!"))))
