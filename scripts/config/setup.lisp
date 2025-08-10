;;;
;;; setup.lisp
;;;


(eval-file "/scripts/config/rooms.lisp")
(eval-file "/scripts/config/damage.lisp")
(eval-file "/scripts/config/timing.lisp")

(setvar "chaos_core_yield_rate" 32)
(setvar "chaos_core_placement_chance" 9)


(setvar "energy_glow_color" 0x66fff7)
(setvar "spr_energy_color_1" 0x94fff9)
(setvar "spr_energy_color_2" 0x006bff)
