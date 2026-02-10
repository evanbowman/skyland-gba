;;;
;;; hostile/imperial_pursuit.lisp
;;;

(dialog
 "Your sensors detect something tracking you. <B:0> "
 "The damaged toll station must have transmitted your signature to the network. <B:0> "
 "A patrol vessel approaches, flying an imperial flag... <B:0> "
 "But something seems off about it.")

(opponent-generate
 (case (zone)
   (0 5)
   (1 8)
   (2 16)
   (else 20)))

(opponent-mode 'neutral)

(flag-show (opponent) flag-id-old-empire)

(defn on-converge ()
  (setq on-converge nil)
  (opponent-mode 'hostile)
  (await (dialog* "As the vessel draws closer, you see the crew through the viewports. <B:0> "
                  "Not imperial officers. Goblins."))
  (await (dialog* "<c:Goblin Raider:32> You! You're the one who broke that sstation! <B:0> "
                  "We sstripped it for partsss after you left! <B:0> "
                  "Got lotsss of good salvage... but now we want YOUR sship too! <B:0> "
                  "Thankss for the help! #cackle#"))
  (await (dialog* "The raiders begin charging weapons...")))
