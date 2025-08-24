;;;
;;; Solar storm event
;;;


(dialog "Your castle passes under a hole in the atmosphere! <B:0>"
        "Be careful! Periodic solar flares may ignite random things! <B:0>"
        "Fire damage is currently increased!")

(adventure-log-add 65 '())

(weather-set weather-id-solar-storm)
