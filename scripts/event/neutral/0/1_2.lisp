;;;
;;; neutral/0/1_2.lisp
;;;


(dialog
 "<b:/scripts/data/img/toll_station.img.bin> "
 "An ancient imperial missile platform contacts you and demands payment...")


(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((hull 0 12)
   (arc-gun 0 10)
   (hull 0 13)
   (hull 0 8)
   (hull 0 9)
   (hull 0 7)
   (hull 0 11)
   (power-core 1 11)
   (hull 1 14)
   (hull 1 7)
   (hull 1 8)
   (power-core 1 9)
   (hull 1 13)
   (hull 2 14)
   (missile-silo 2 7)
   (hull 2 13)
   (hull 3 7)
   (hull 3 8)
   (hull 3 9)
   (hull 3 10)
   (hull 3 11)
   (hull 3 12)
   (hull 3 13)
   (rocket-bomb 4 8)
   (hull 4 11)))

(flag-show (opponent) flag-id-old-empire)


(defn/temp sabotage-station ()
  (await (dialog*
          "Your crew targets the station's external sensors with precision fire. <B:0> "
          "The weapons array remains active, but its targeting systems go dark. <B:0> "
          "You slip past the blind station without incident."))
  (adventure-log-add 77 '())
  (push-pending-event (+ 3 (choice 3)) "/scripts/events/hostile/imperial_pursuit.lisp")
  (exit))


(defn/temp attack-player ((text . string))
  (opponent-mode 'hostile)
  (await (dialog* text)))


(defn/temp pay-toll (receipt)
  (if (< (coins) 600)
      (progn
        (adventure-log-add 59 '())
        (attack-player "Insufficient funds! The station begins charging its weapons!"))
      (progn
        (adventure-log-add 60 (list 600))
        (coins-add -600)
        (when receipt
          (await (dialog*
                  "The station processes your payment. <B:0> "
                  "After a moment, it transmits a receipt datapacket to your systems."))
          (pickup-cart 12 "Your crew examines the transmission: a properly formatted Imperial Transit Voucher, complete with authentication seals and tax filing instructions. <B:0> According to the attached guidance, this 600@ toll is fully deductible from your annual merchant levy... <B:0> ...which was last collected seventeen years ago."))
        (await (dialog* "The station deactivates and allows you to pass."))
        (exit))))


(defn/temp investigate ()
  (await (dialog*
    "Your crew examines the station more closely. <B:0> "
    "The toll demand references Imperial Transit Code section 47-G, subsection 12… <B:0> "
    "…which was amended by decree 891 in the Third Solar Cycle… <B:0> "
    "…but that decree was suspended pending review after the empire fractured…"))

  (await (dialog*
    "Whether this toll is even legally enforceable would require months of archival research. <B:0> "
    "Your engineer has a simpler solution: physically disable the station's sensor array. <B:0> "
    "It won't know you passed. But someone will eventually notice the damage…"))

  (let ((sel (await (dialog-choice* "Disable the sensors?"
                                    '("Vandalize it."
                                      "Just pay the toll."
                                      "Refuse and fight!")))))
    (case sel
      (0 (sabotage-station))
      (1 (pay-toll true))
      (2 (on-dialog-declined)))))


(defn on-converge ()
  (setq on-converge nil)
  (let ((sel (await
              (dialog-choice*
               (string "While the old empire is now fragmented and most of its weapons systems "
                       "are offline, this automated vessel seems to still be functioning. <B:0>"
                       "The station's computers demand a toll of 600@. Pay?")
               '("Pay 600@ toll."
                 "Examine the station…"
                 "Refuse.")))))
    (case sel
      (0 (pay-toll false))
      (1 (investigate))
      (2 (on-dialog-declined)))))


(defn on-dialog-declined ()
  (attack-player "The station begins charging its weapons!"))
