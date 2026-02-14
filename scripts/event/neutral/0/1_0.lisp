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

(flag-show (opponent) flag-id-marauder)


(defn/temp attack-player (msg)
  (await (dialog* msg))
  (await (dialog* "<c:goblin:2>Yesss captain!"))
  (await (dialog* "(the transmission was cut)"))
  (opponent-mode 'hostile))


(defn/temp handle-bluff ()
  ;; You accused redbeard of bluffing. He fires a broadside off your bow.
  (sleep 400)
  (emit (opponent) 0 12 (terrain (player)) 0)
  (sleep 200)
  (emit (opponent) 0 13 (terrain (player)) 0)
  (sleep 200)
  (emit (opponent) 0 14 (terrain (player)) 0)
  (sleep 1200)
  (let ((sel (await (dialog-choice* "<c:Redbeard:12>Yaargh!! I'm just a simple marauder, trying to earn a decent living here! [via petty extortion, how else?] <B:0> So what's it gonna be? Last chance..."
                                    '("Pay 600@."
                                      "Fight back.")))))
    (case sel
      (0 (on-dialog-accepted))
      (1 (on-dialog-declined)))))



(defn/temp try-negotiate ()
  (let ((sel (await (dialog-choice* (string "<c:Redbeard:12>Negotiate!? Ha. <d:1000> Hahahah! "
                                            "<d:1000> The only negotiation will be how fast you "
                                            "pay me!")
                                    '("Here's 600@…"
                                      "You're bluffing!"
                                      "Never!")))))
    (case sel
      (0 (on-dialog-accepted))
      (1 (handle-bluff))
      (2 (on-dialog-declined)))))



(defn on-converge ()
  (setq on-converge nil)
  (let ((sel (await (dialog-choice* "<c:Redbeard:12>Aarrrgh!! You're trespassing in my domain. Gimme 600@ or I'll blast your island to bits!"
                                    '("Here's 600@…"
                                      "You're bluffing!"
                                      "Let's negotiate!"
                                      "Never!")))))
    (case sel
      (0 (on-dialog-accepted))
      (1 (handle-bluff))
      (2 (try-negotiate))
      (3 (on-dialog-declined)))))



(defn on-dialog-accepted ()
  (if (< (coins) 600)
      (progn
        (adventure-log-add 12 '())
        (attack-player "<c:redbeard:12>That's not enough, load the cannons!!!"))
      (progn
        (adventure-log-add 13 (list 600))
        (coins-add -600)
        (dialog "<c:redbeard:12>Heh. I think you made the smart decision.")
        (exit))))


(defn on-dialog-declined ()
  (adventure-log-add 14 '())
  (attack-player "<c:redbeard:12>Whaatt!! Load the cannons!!!"))


(defn on-room-destroyed (isle sym)
  ;; TODO: rewrite this with new-style await pattern
  (if (and (equal isle (opponent))
           (equal 2 (+ (room-count (opponent) 'cannon)
                       (room-count (opponent) 'missile-silo))))
      (progn
        (setq on-room-destroyed nil)
        (opponent-mode 'neutral)
        (await (dialog* "<c:Redbeard:12>Alright, alright! You've bested me! I yield! <B:0> You've disarmed the great Redbeard without sinking him - not many can claim that. <B:0> Take what you want from my hold, just... leave me vessel intact, aye?"))
        (pickup-cart 11 "Among the scattered coins and looted cargo, you find a battered data cartridge, its label barely legible. <B:0> Redbeard waves dismissively. 'Old stories, mostly lies. Take it if you want.'")
        (coins-add (coins-victory))
        (exit 2))))
