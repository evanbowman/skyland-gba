;;;
;;; quest_marker/atomics.lisp
;;;


(dialog
 "<b:/scripts/data/img/sylph_city_1.img.bin>"
 "You arrive at Aestria, the destination indicated by the conclave elder...")


(opponent-init 14 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 12) (particle-lance 0 14) (bronze-hull 0 13) (shrubbery 0 11) (masonry 1 12 0) (bronze-hull 1 13) (lemon-tree 2 11) (bronze-hull 2 13) (masonry 3 13 0) (masonry 3 14 0) (war-engine 3 9) (deflector 3 8) (masonry 4 14 0) (masonry 5 13 0) (bridge 5 8 30 1) (masonry 5 14 0) (deflector 6 7) (water-source 6 14) (power-core 6 11) (bridge 6 8 30 1) (bridge 7 8 30 1) (deflector 7 7) (water-source 7 14) (bridge 8 8 30 1) (masonry 8 14 0) (masonry 8 13 0) (war-engine 8 9) (masonry 9 14 0) (masonry 10 13 0) (deflector 10 8) (masonry 10 14 0) (bronze-hull 11 14) (bronze-hull 11 13) (bronze-hull 12 14) (water-source 12 13) (bronze-hull 13 14) (bronze-hull 13 13) (bronze-hull 13 12)))


(defn on-converge ()
  (setq on-converge nil)
  (dialog "<c:Conclave Elder:51>Aestria was among the first cities we lifted from the mountains. <B:0> The archive lights are dark now... sad to see it like this.")

  (defn on-dialog-closed ()
    (dialog "<c:Conclave Elder:51> The Conclave believes documentation will save us. Careful observation. Measured response. <B:0> But our cities go dark while we _debate_. <B:0> The particle lance... I never meant for you to merely retrieve it...")
    (defn on-dialog-closed ()
      (dialog "<c:Conclave Elder:51>This device can cut through anything. Focused properly, it could pierce the dark castle within the storm. <B:0> I brought this plan to the Conclave. They called it reckless. Unproven. <B:0> But there is no time left for study, we must act!")
      (defn on-dialog-closed ()

        (alloc-space 'particle-lance)

        (sel-input 'particle-lance
                   "Place weapon (1x3)"
                   (lambda (isle x y)
                     (foreach (lambda (r)
                                (when (equal (car r) 'particle-lance)
                                  (room-del (opponent) (get r 1) (get r 2))))
                              (rooms (opponent)))
                     (room-new (player) (list 'particle-lance x y))
                     (adventure-log-add 64 '())
                     (sound "build0")
                     (dialog "You retrieved the particle-lance!")
                     (setq on-dialog-closed exit)))))))
