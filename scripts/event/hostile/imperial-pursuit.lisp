;;;
;;; hostile/imperial_pursuit.lisp
;;;

(dialog
  "A sleek imperial patrol craft drops from the upper atmosphere, moving with clear purpose. <B:0> "
  "Unlike the automated toll station, this one has a crew...")

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
  (foreach (lambda (chr)
             (let ((x (get chr 0))
                   (y (get chr 1)))
               (chr-del (opponent) x y)
               (chr-new (opponent) x y 'hostile '((race . 2)))))
           (chrs (opponent)))
  (opponent-mode 'hostile)
  (dialog
    "<c:Imperial Inspector:55> Automated checkpoint seven-nine reported sensor damage consistent with weapons fire. <B:0> "
    "Surveillance logs show your vessel was the only craft in range during the incident. <B:0> "
    "Destruction of imperial infrastructure carries a mandatory enforcement response. <B:0> "
    "You will submit to boarding and asset seizure."))
