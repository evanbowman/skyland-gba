;;;
;;; setup.lisp
;;;


(eval-file "/scripts/config/rooms.lisp")
(eval-file "/scripts/config/damage.lisp")
(eval-file "/scripts/config/timing.lisp")

(configure-vars
 '((32 . "chaos_core_yield_rate")
   (9  . "chaos_core_placement_chance")
   (0x66fff7 . "energy_glow_color")
   (0x94fff9 . "spr_energy_color_1")
   (0x006bff . "spr_energy_color_2")))
