;;;
;;; hostile/scavenger-vengeance.lisp
;;;

(dialog "A heavily armed goblin vessel emerges from the clouds, weapons already charging... <B:0> You recognize the distinctive markings on their hull - these are the same scavengers you robbed earlier! <B:0> They've returned with reinforcements and better equipment, eager for revenge...")


(opponent-generate
 (case (zone)
   (0 5)
   (1 8)
   (2 16)
   (else 20)))


(opponent-mode 'neutral)



(defn on-converge()
  (dialog "<c:Scavenger:35>Ssso... we meet again! <B:0> Did you think we'd just forget what you did to usss? <B:0> We've spent hours gathering friendsss and better weaponsss. <B:0> Now, you can pay us back... with interessst!")
  (setq on-converge nil)

  (dialog-opts-reset)
  (when (> (coins) 2799)
    (dialog-opts-push "Pay 2800@."
                      (lambda ()
                        (coins-add -2800)
                        (adventure-log-add 72 (list 2800))
                        (dialog "<c:Scavenger:35>Hah! Sssmart choice! <B:0> You humans learn quickly when your livesss are on the line. <B:0> Keep that lessson in mind next time you think about crossing usss!")
                        (defn on-dialog-closed ()
                          (dialog "The scavengers take your payment and retreat into the clouds, cackling with satisfaction...")
                          (setq on-dialog-closed exit)))))

  (dialog-opts-push "Mock their cowardice."
                    (lambda ()
                      (adventure-log-add 73 nil)
                      (dialog "<c:Scavenger:35>COWARDICE?! <B:0> We'll show you cowardice, you arrogant fool! <B:0> TEAR THEM APART! Leave nothing but scrap metal floating in the cloudsss!")
                      (opponent-mode 'hostile)))

  (dialog-opts-push "Refuse and fight."
                    (lambda ()
                      (adventure-log-add 73 nil)
                      (dialog "<c:Scavenger:35>Should have taken the easy way out... <B:0> No more talksss! Attack!")
                      (defn on-dialog-closed ()
                        (dialog "The goblin scavengers move into battle formation, weapons charging...")
                        (setq on-dialog-closed nil))
                      (opponent-mode 'hostile))))
