;;;
;;; timing.lisp
;;;


(foreach (lambda (kvp) (setvar (second kvp) (first kvp)))
 '((25000 . "decimator_reload_ms")
   (25000 . "drone_bay_reload_ms")
   (18000 . "transporter_reload_ms")
   (10000 . "beam_reload_ms")
   (6800 . "incinerator_reload_ms")
   (3500 . "cannon_reload_ms")
   (4200 . "fire_charge_reload_ms")
   (4000 . "nemesis_reload_ms")
   (7000 . "missile_silo_reload_ms")
   (4500 . "flak_gun_reload_ms")
   (3500 . "ion_cannon_reload_ms")
   (3500 . "arc_gun_reload_ms")))
