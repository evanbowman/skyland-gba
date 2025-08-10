;;;
;;; challenges/mycelium.lisp
;;;


(defn on-victory ()
  (challenge-complete 4)
  (achieve 18))


(let ((cnt 0))
  (defn challenge-hint ()
    (dialog (if (equal cnt 0)
                "Are you sure you want a hint?"
              "Need another hint?"))
    (dialog-await-y/n)

    (defn on-dialog-accepted ()
      (dialog
       (cond
        ((equal cnt 0)
         (+= cnt 1)
         "Hint: Mycelium won't grow on forcefields, and is weak against arc-guns.")
        ((equal cnt 1)
         (+= cnt 1)
         "Hint: You can incapacitate the cannon by destroying the two forcefields in front of it, allowing the mycelium to grow over the cannon.")
        ((equal cnt 2)
         (setq cnt 0)
         "OK, one final hint: After building anything that you need from the manufactory, salvage it for resources."))))


    (setq on-dialog-declined (lambda ()))))


(terrain-set (player) 7)
(island-configure
 (player)
 '((power-core 1 13)
   (manufactory 3 13)))

(flag-show (player) 0)

(chr-new (player) 2 14 'neutral nil)

(coins-add 8000)


(opponent-init 9 'hostile)


(island-configure
 (opponent)
 '((mycelium 0 10)
   (mycelium 0 11)
   (hull 0 7)
   (mycelium 0 9)
   (forcefield 0 14)
   (mycelium 0 6)
   (mycelium 0 8)
   (forcefield 0 13)
   (mycelium 1 6)
   (mycelium 1 5)
   (cannon 1 14)
   (mycelium 1 8)
   (hull 1 10)
   (mycelium 1 11)
   (forcefield 1 12)
   (mycelium 1 9)
   (mycelium 1 13)
   (mycelium 1 7)
   (mycelium 2 8)
   (mycelium 2 10)
   (hull 2 14)
   (hull 2 13)
   (hull 2 12)
   (missile-silo 2 6)
   (forcefield 2 5)
   (mycelium 2 11)
   (mycelium 2 9)
   (power-core 3 13)
   (hull 3 12)
   (mycelium 3 5)
   (mycelium 3 6)
   (mycelium 3 8)
   (mycelium 3 9)
   (mycelium 3 7)
   (power-core 3 10)
   (forcefield 4 5)
   (radiator 4 12)
   (missile-silo 4 6)
   (hull 4 9)
   (mycelium 4 8)
   (mycelium 5 6)
   (mycelium 5 8)
   (hull 5 12)
   (mycelium 5 10)
   (mycelium 5 9)
   (mycelium 5 11)
   (mycelium 5 7)
   (mycelium 5 5)
   (power-core 5 13)
   (mycelium 6 11)
   (mycelium 6 9)
   (power-core 6 7)
   (hull 6 12)
   (mycelium 6 5)
   (hull 6 6 40)
   (mycelium 6 4)
   (mycelium 7 12)
   (mycelium 7 11)
   (missile-silo 7 5)
   (mycelium 7 9)
   (forcefield 7 4)
   (power-core 7 13)
   (mycelium 8 4)
   (mycelium 8 5)
   (mycelium 8 8)
   (mycelium 8 7)
   (mycelium 8 6)
   (mycelium 8 9)
   (mycelium 8 12)))


(weather-set 3)
