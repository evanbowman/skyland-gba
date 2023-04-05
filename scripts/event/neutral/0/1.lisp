;;;
;;; neutral/0/1.lisp
;;;


(dialog "A heavily armed pirate fortress approaches. Its captain demands to speak with you.")


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


(setq on-converge
      (lambda
        (dialog "<c:redbeard:13>Aarrrgh!! You're tresspassing in my domain. Gimme 600@ or I'll blast your island to bits!")
        (dialog-await-y/n)
        (setq on-converge nil)))


(let ((scr
       (lambda
         (dialog $0)
         (defn on-dialog-closed
           (dialog "<c:goblin:2>Yesss captain!")
           (defn on-dialog-closed
             (dialog "(the transmission was cut)")
             (setq on-dialog-closed nil)))
         (opponent-mode 'hostile))))
  (setq on-dialog-accepted
        (lambda
          (if (< (coins) 600)
              (scr "<c:redbeard:13>That's not enough, load the cannons!!!")
            (progn
              (coins-add -600)
              (dialog "<c:redbeard:13>Heh. I think you made the smart decision.")
              (exit)))))


  (setq on-dialog-declined
        (lambda
          (scr "<c:redbeard:13>Whaatt!! Load the cannons!!!"))))
