;;;
;;; timing.lisp
;;;


(map (lambda (syscall "setvar" (cdr $0) (car $0)))
 '((25000 . "decimator_reload_ms")
   (25000 . "drone_bay_reload_ms")
   (18000 . "transporter_reload_ms")
   (3500 . "cannon_reload_ms")
   (4200 . "fire_charge_reload_ms")
   (4000 . "nemesis_reload_ms")
   (7000 . "missile_silo_reload_ms")
   (4500 . "flak_gun_reload_ms")
   (3500 . "ion_cannon_reload_ms")
   (3500 . "arc_gun_reload_ms")))