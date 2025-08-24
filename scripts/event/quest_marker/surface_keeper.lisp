;;;
;;; quest_marker/surface_keeper.lisp
;;;


(dialog "You reach the destination that the surface keeper marked on your map! <B:0> Beyond the clouds, pulses of strange energy light up the toxic atmosphere...")


(let ((keeper-id (lookup 5 qvar))
      (id nil))
  (foreach (lambda (chr)
             (if (equal keeper-id (lookup 'id (cddr chr)))
                 (setq id (lookup 'id (cddr chr)))))
           (chrs (player)))

  (defn on-converge ()
    (setq on-converge nil)
    (if id
        (progn
          (let ((tab (eval-file "/scripts/config/room_tab.lisp"))
                (sel '()))

            (while (< (length sel) 1)
              (let ((pick (sample tab)))
                (let ((cost (get pick 2)))
                  (when (> cost 1000)
                    (setq sel (cons (car pick) sel))))))

            (dialog "<c:Surface Keeper:43>The readingsss grow stronger... <B:0> Beneath usss lies a cache of ancient weaponsss, their containment failing. <B:0> I must descend to stabilize them, before their radiation spreadsss upward. <B:0> Take thisss payment for your aid. (2000@ and one random block) ")
            (defn on-dialog-closed ()
              (coins-add 2000)
              (let ((sym0 (get sel 0)))
                (alloc-space sym0)
                (sel-input sym0
                           (string "Place " (rinfo 'name sym0))
                           (lambda (isle x y)
                             (sound "build0")
                             (room-new (player) (list sym0 x y))
                             (dialog "<c:Surface Keeper:43>The ancient machinesss require my attention...")
                             (map (lambda (chr)
                                    (if (equal id (lookup 'id (cddr chr)))
                                        (chr-del (player) (car chr) (cadr chr))))
                                  (chrs (player)))
                             (defn on-dialog-closed ()
                               (dialog "The surface keeper departed!")
                               (setq on-dialog-closed exit))))))))
      (progn
        (dialog "Unfortunately, it seems the surface keeper is no longer aboard your island...")
        (exit)))))


(weather-set weather-id-ash)

(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((energized-hull 0 11) (energized-hull 0 14) (energized-hull 0 10) (energized-hull 0 9) (energized-hull 0 12) (energized-hull 0 13) (energized-hull 1 8) (energized-hull 1 9) (hull 1 11) (hull 1 13) (hull 1 12) (hull 1 10) (hull 1 14) (energized-hull 2 8) (power-core 2 13) (reactor 2 10) (hull 2 9) (hull 3 9) (energized-hull 3 8) (hull 4 10) (energized-hull 4 8) (energized-hull 4 9) (hull 4 11) (hull 4 12) (hull 4 13) (hull 4 14) (energized-hull 5 14) (energized-hull 5 13) (energized-hull 5 12) (energized-hull 5 11) (energized-hull 5 10) (energized-hull 5 9)))
