;;;
;;; tutorials.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")
(setvar "powerdown_allowed" 1)

;; NOTE: the list should not exceed 64 tutorials.


(append '(("Introduction" "tutorials/overview.lisp" 0)
          ("Game Speed" "tutorials/game_speed.lisp" 1)
          ("Weapon Groups" "tutorials/weapon_groups.lisp" 2)
          ("Firing Patterns" "tutorials/firing_patterns.lisp" 3)
          ("Target Queueing" "tutorials/target_queueing.lisp" 4)
          ("Salvage" "tutorials/salvage.lisp" 5)
          ("Moving Blocks" "tutorials/moving_blocks.lisp" 6)
          ("Construction Tricks" "tutorials/construction_tricks.lisp" 7)
          ("Advanced Structures" "tutorials/advanced_structures.lisp" 8)
          ("Your Crew"  "tutorials/characters.lisp" 9)
          ("Replicators" "tutorials/replicator.lisp" 10)
          ("Transporters" "tutorials/transporter.lisp" 11)
          ("Portals" "tutorials/portals.lisp" 15)
          ("Adding Terrain" "tutorials/terrain.lisp" 12)
          ("Power Balance" "tutorials/power_balance.lisp" 13)
          ("Diverting Power" "tutorials/power_balance_2.lisp" 14))
        (if (is-developer-mode) ; In developer mode, append the extra tutorial, otherwise, append nothing
            '(("template" "tutorials/template.lisp" 16))
            nil))
