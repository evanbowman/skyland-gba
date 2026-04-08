;;;
;;; Ash storm event
;;;

(tr-bind-current)

(dialog (tr "Unexpected bad weather forces your island to retreat below the clouds. <B:0> ")
        "<b:/scripts/data/img/radiation_warning.img.bin>"
        (tr "Heavy particles and radioactive ash blow through the air, periodically damaging exposed areas of your castle. <B:0>")
        "<b:/scripts/data/img/murk.img.bin>"
        (tr "Just when things couldn't seem to get any worse, an enemy raiding ship, also sheltering from the storm, emerges from the murk..."))

(weather-set weather-id-ash)
(adventure-log-add 56 nil)
