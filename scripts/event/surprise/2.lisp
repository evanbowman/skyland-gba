;;;
;;; Solar storm event
;;;

(tr-bind-current)


(dialog (tr "Your castle passes under a hole in the atmosphere! <B:0>")
        (tr "Be careful! Periodic solar flares may ignite random things! <B:0>")
        (tr "Fire damage is currently increased!"))

(adventure-log-add 65 '())

(weather-set weather-id-solar-storm)
