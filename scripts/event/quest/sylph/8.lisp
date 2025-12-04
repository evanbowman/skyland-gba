;;;
;;; quest/sylph/8.lisp
;;;


(dialog
 "<b:/scripts/data/img/ornate.img.bin>"
 "An impossibly large Sylph city emerges from the clouds. Before you can react, your radio crackles with coordinates and docking instructions... <B:0> "
 "<b:/scripts/data/img/sylph_conclave.img.bin>"
 "You're guided into the innermost chambers, where you meet the Sylph High Conclave...")


(opponent-init 10 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 14) (bronze-hull 1 13) (shrubbery 1 12) (masonry 1 14 0) (masonry 2 14 0) (power-core 2 11) (masonry 3 14 0) (power-core 4 8) (power-core 4 12) (masonry 4 14 0) (power-core 4 10) (masonry 5 14 0) (masonry 6 14 0) (reactor 6 10) (masonry 6 13 0) (windmill 7 14) (masonry 7 13 0) (masonry 8 14 0) (masonry 8 13 0) (bronze-hull 8 12) (lemon-tree 8 10) (shrubbery 9 14)))



(defn on-converge ()
  (dialog
   "<c:Conclave Elder:51>We have monitored your vessel's progress, your fortress demonstrates... adaptability. <B:0> This quality is required for a matter of immediate concern. <B:0>"
   "City Aestria has ceased all transmissions. Core crystal failure - complete power loss. <B:0> The city carried a particle lance, mining technology from our mountain era. In trained hands, merely a tool. <B:0> In desperate or ignorant hands... the device destabilizes catastrophically. <B:0>"
   "We cannot allow scavengers to claim what remains. You will retrieve the particle lance before the city falls into the wrong hands.")

  (dialog-await-binary-q-w/lore
   "We'll retrieve it."
   "Sorry, we can't."
   '(("Particle lance?" .
      "<c:Conclave Elder:51>An adapted bore-beam designed for extracting ore from dense mountain deposits. <B:0> Its focused particle stream can cut through virtually any material. <B:0> If mishandled or damaged, the containment field collapses. The resulting energy release is... significant.")
     ("Why us?" .
      "<c:Conclave Elder:51>Our own vessels are engaged in evacuations and core stabilization efforts across multiple cities. <B:0> You are capable, and more importantly, you are proximate. <B:0> Time is not on our side.")))

  (defn on-dialog-accepted ()
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
      (setq on-dialog-closed exit)
      (if m
          (progn
            (adventure-log-add 63 '())
            (push 'qids 8)
            (push 'quests (cons "particle_lance.lisp" m))
            (dialog "<c:Conclave Elder:51>The coordinates have been transmitted to your navigation system. <B:0> I will accompany you personally to ensure proper handling of the device.")
            (defn on-dialog-closed ()
              (setq on-dialog-closed nil)
              (run-util-script
               "find-crew-slot"
               "<c:Conclave Elder:51>Your fortress layout is... unconventional. I will need a position from which to work."
               'ladder
               "Place block (1x2):"
               (lambda (x y _)
                 (chr-new (player) x y 'neutral '((icon . 51) (race . 4)))
                 (dialog "The Conclave Elder joined your crew!")
                 (defn on-dialog-closed ()
                   (exit))))))
          (progn
            (dialog "<c:Conclave Elder:51>Unfortunate. The navigation data indicates you lack sufficient range to reach City Aestria before the storm front overtakes it. <B:0> We will seek alternative solutions.")))))

  (defn on-dialog-declined ()
    (dialog "<c:Conclave Elder:51>Your caution is noted...")
    (setq on-dialog-closed exit)))
