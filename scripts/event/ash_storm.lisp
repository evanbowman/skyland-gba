
(dialog "Unexpected bad weather forces your island to retreat below the clouds. <B:0> "
        "<b:/scripts/misc/img/radiation_warning.img.bin>"
        "Heavy particles and radioactive ash blow through the air, periodically damaging "
        "all exposed areas of your castle. <B:0>"
        "<b:/scripts/misc/img/murk.img.bin>"
        "Just when things couldn't seem to get any worse, an enemy raiding ship, "
        "also forced below the clouds, emerges from the murk...")

(weather 6)
(adventure-log-add 56 nil)
(adv-var-store "ash-storm-count" (+ sc 1))
