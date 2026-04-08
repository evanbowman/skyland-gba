;;;
;;; tutorials.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")

(tr-bind-current)

(configure-vars
 '((1 . "powerdown_allowed")
   (0 . "rewind_disabled")
   (0 . "cold_boot_penalty_ms")))

;; NOTE: The list should not exceed 64 tutorials.
(append `((,(tr "Introduction") "tutorials/overview.lisp" 0)
          (,(tr "Game Speed") "tutorials/game_speed.lisp" 1)
          (,(tr "Weapon Groups") "tutorials/weapon_groups.lisp" 2)
          (,(tr "Firing Patterns") "tutorials/firing_patterns.lisp" 3)
          (,(tr "Target Queueing") "tutorials/target_queueing.lisp" 4)
          (,(tr "Salvage") "tutorials/salvage.lisp" 5)
          (,(tr "Moving Blocks") "tutorials/moving_blocks.lisp" 6)
          (,(tr "Construction Tricks") "tutorials/construction_tricks.lisp" 7)
          (,(tr "Advanced Structures") "tutorials/advanced_structures.lisp" 8)
          (,(tr "Your Crew")  "tutorials/characters.lisp" 9)
          (,(tr "Replicators") "tutorials/replicator.lisp" 10)
          (,(tr "Transporters") "tutorials/transporter.lisp" 11)
          (,(tr "Portals") "tutorials/portals.lisp" 15)
          (,(tr "Adding Terrain") "tutorials/terrain.lisp" 12)
          (,(tr "Power Balance") "tutorials/power_balance.lisp" 13)
          (,(tr "Diverting Power") "tutorials/power_balance_2.lisp" 14))
        (if (is-developer-mode) ;; In developer mode, append the extra tutorial, otherwise, append nothing.
            '(("template" "tutorials/template.lisp" 16))
            nil))
