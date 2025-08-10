;;;
;;; quest/goblin/5.lisp
;;;


(dialog
 "<b:/scripts/data/img/surface_keeper.img.bin>"
 "Your sensors detect movement in the radioactive clouds below... <B:0> A figure emerges, wrapped in thick robes and wearing elaborate breathing apparatus. <B:0> They signal your fortress from their floating observation post...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((hull 0 14) (hull 0 13) (masonry 1 14 3) (masonry 2 14 3) (deflector 2 12) (hull 2 13) (war-engine 3 11) (masonry 6 14 3) (hull 6 13) (deflector 6 12) (masonry 7 14 3) (hull 8 14) (hull 8 13)))


(chr-new (opponent) 1 13 'neutral '((race . 1)))


(defn on-converge ()
  (setq on-converge nil)

  (dialog "<c:Surface Keeper:43>The cloudsss thin here... allowing usss to reach the sssky dwellersss. <B:0> Our instruments detect... disturbing energy readingsss in the ruinsss below. <B:0> Something awakensss. We require aid from those who command flying fortressesss.")

  (dialog-await-binary-q "I accept!" "I'm kind of busy…")

  (defn on-dialog-accepted ()
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
      (when m
        (push 'qids 5)
        (adventure-log-add 52 '())
        (push 'quests (cons "surface_keeper.lisp" m)))

      (run-util-script
       "find-crew-slot"
       "<c:Surface Keeper:43>No ssspace! Let's ssseee..."
       'ladder
       "Place block (1x2):"
       (lambda (x y _)
         (chr-del (opponent) 1 13)
         (let ((id (chr-new (player) x y 'neutral '((race . 1) (icon . 43)))))
           (push 'qvar (cons 5 id)))

         (dialog (if m
                     "<c:Surface Keeper:43>Yesss... time is critical. I've marked the sssource of the disturbance on your chart with an *."
                     "<c:Surface Keeper:43>The energy readingsss have grown too unstable... We can no longer approach sssafely. The containment mussst have already failed..."))
         (defn on-dialog-closed ()
           (dialog "The surface keeper goblin joined your crew!")
           (setq on-dialog-closed exit))))))


  (defn on-dialog-declined ()
    (exit)))
