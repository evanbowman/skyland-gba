;;;
;;; neutral/2/1_alt0.lisp
;;;


(dialog "A damaged fortress floats into view. The residents do not respond to your radio signals.")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 2 13)
   (hull 2 14)
   (hull 3 12)
   (hull 4 12)))


(chr-new (opponent) 1 14 'neutral
         (list (cons 'race (if (equal (faction) 'goblin) 1 0))))


(defn on-converge ()
  (setq on-converge nil)
  (if (dialog-await-y/n (string
                         "You see a survivor amongst the wreckage. You cannot be sure whether the"
                         " survivor is trustworthy. Invite survivor aboard?"))
      (on-dialog-accepted)
      (on-dialog-declined)))


(let ((bad (choice 2)))

  (if (< (zone) 2)
      (secret
       4 12
       (if bad
           "Humans eaten: 17"
         "Days alone on island: lll")))


  (defn/temp join-good (slot)
    (chr-new (player) (car slot) (cdr slot) 'neutral
             (list (cons 'race
                         (if (equal (faction) 'goblin) 1 0))))
    (await (dialog* "The survivor joined your crew!"))
    (adventure-log-add 40 '())
    (exit))


  (defn/temp join-bad (slot)
    (opponent-mode 'hostile)
    (chr-new (player) (car slot) (cdr slot) 'hostile nil)
    (await (dialog*
            (if (equal (faction) 'goblin)
                "The survivor turned out to be very unfriendly!"
                "The survivor turned out to be a vicious goblin!")))
    (adventure-log-add 41 '())
    (await (dialog* "<c:Goblin:2>Die "
                    (case (faction)
                      ('goblin "Traitorsss")
                      ('human "Humansss")
                      ('sylph "Sssylph ssscum"))
                    "!")))


  (defn on-dialog-accepted ()
    (let ((slots (chr-slots (player))))
      (if slots
          (progn
            (chr-del (opponent) 1 14)
            (if (not bad)
                (join-good (car slots))
                (join-bad (car slots))))
          (progn
            (dialog "Sadly, there's no room...")
            (exit))))))


(defn on-dialog-declined ()
  (exit))
