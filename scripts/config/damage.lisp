;;;
;;; damage.lisp
;;;


(foreach (lambda (kvp) (setvar (second kvp) (first kvp)))
 '((190 . "decimator_burst_damage")
   (25  . "nemesis_blast_damage")
   (40  . "cannonball_damage")
   (60  . "ballista_damage")
   (10  . "ballista_splash_damage")
   (120 . "ion_burst_damage")
   (36  . "firebolt_damage")
   (36  . "beam_damage")
   (100 . "missile_damage")
   (20  . "flak_r1_damage")
   (12  . "flak_r2_damage")
   (10  . "flak_r3_damage")
   (20  . "arcbolt_damage")
   (10   . "incinerator_impact_damage")
   (2   . "incinerator_splash_damage")
   (40  . "rocket_bomb_impact_damage")
   (16  . "rocket_bomb_splash_damage")))

(setvar "block_crack_threshold_health" 60)
(setvar "deflector_shield_strength" 8)
(setvar "dust_storm_damage" 8)
(setvar "sylph_cannon_damage_percent" 25)
