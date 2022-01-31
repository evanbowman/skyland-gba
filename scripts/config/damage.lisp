;;;
;;; damage.lisp
;;;


(map (lambda (setvar (cdr $0) (car $0)))
 '((190 . "decimator_burst_damage")
   (25  . "vendetta_blast_damage")
   (40  . "cannonball_damage")
   (120 . "ion_burst_damage")
   (100 . "missile_damage")
   (20  . "flak_r1_damage")
   (12  . "flak_r2_damage")
   (10  . "flak_r3_damage")
   (20  . "arcbolt_damage")))
