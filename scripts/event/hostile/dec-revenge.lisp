;;;
;;; hostile/dec-revenge.lisp
;;;


(dialog "A goblin war party intercepts your route, their ships bristling with weapons...")


(defn on-fadein ()
  (dialog
   (if (room-count (player) 'decimator)
       "<c:Goblin Captain:43>There! That'sss our decimator! <B:0> We've been hunting that weapon for weeksss! <B:0> No talksss, no dealsss - we're taking it back! <B:0> ATTACK!"
       "<c:Goblin Captain:43>You! We know you have our ssstolen decimator! <B:0> Don't try to hide it - we tracked the energy sssignature to YOUR ship! <B:0> Where isss it?! No matter... we'll tear your cassstle apart until we find it! <B:0> ATTACK!")))


(adventure-log-add 75 '())


(opponent-generate
 (case (zone)
   (0 5)
   (1 8)
   (2 16)
   (else 20)))
