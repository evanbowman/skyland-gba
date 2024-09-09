;;;
;;; damage.lisp
;;;


(map (lambda (kvp) (setvar (second kvp) (first kvp)))
 '((190 . "decimator_burst_damage")
   (25  . "nemesis_blast_damage")
   (40  . "cannonball_damage")
   (120 . "ion_burst_damage")
   (36  . "firebolt_damage")
   (100 . "missile_damage")
   (20  . "flak_r1_damage")
   (12  . "flak_r2_damage")
   (10  . "flak_r3_damage")
   (20  . "arcbolt_damage")))

(setvar "block_crack_threshold_health" 60)
