;;;
;;; neutral/0/0_sylph.lisp
;;;

(dialog "In the distance, you see an island inhabited by a lone engineer...")


(opponent-init 6 'neutral)
(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 12)
   (hull 5 14)))

(secret  5 14
         "Emergency Log: Core crystal failure. Day 6. Radio damaged. Supply low.")


(chr-new (opponent) 1 14 'neutral '((race . 4)))


(defn on-converge ()
  (dialog
   "<c:Sylph Engineer:48>Finally! I've been stuck here since my core crystal cracked. <B:0> It's happening everywhere - the old crystals are deteriorating. Something about the changing atmosphere. <B:0> We had goblins sniffing around yesterday, probably smelled the dead power signature. Had to hide in the lower decks until they lost interest. <B:0> Look, I can work. I know systems. Just... need to get somewhere with a functioning workshop.")

  (setq on-dialog-closed
        (lambda ()
          (dialog "She seems capable, invite her aboard?")
          (dialog-await-binary-q-w/lore "Welcome aboard!" "Not today."
                                        '(("crystal deterioration?" .
                                           "<c:Sylph Engineer:48>The crystals were harvested from our mountain homes, back before the migration. <B:0> They worked perfectly for centuries. But something in the upper atmosphere is breaking them down - micro-fractures spreading through the matrix. <B:0> We don't have access to the source veins anymore. Can't grow new ones. <B:0> Every city is rationing power now, trying to make the old crystals last.")
                                          ("the goblins?" .
                                           "<c:Sylph Engineer:48>They showed up about a day after my core failed. Prowling around, testing for weak spots. <B:0> I shut down all systems, made it look abandoned. They poked around for a while, took some supplies, then moved on. <B:0> They're getting bolder. They know we're vulnerable now.")))
          (setq on-dialog-closed '())))

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (run-util-script
   "find-crew-slot"
   "<c:Sylph Engineer:48>Wait - you're full up. Let me help. I can optimize your layout, make room."
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (adventure-log-add 7 '())
     (chr-del (opponent) 1 14)
     (chr-new (player) x y 'neutral '((race . 4) (icon . 48)))
     (dialog "<c:Sylph Engineer:48>Thank you. Truly. <B:0> ... <B:0> Your core is in better shape than I expected. Old human design, yes, but maintained well. <B:0> I can help keep it running. It's... it feels good to have systems to work on again. Purpose. <B:0> Thank you for that.")
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (dialog "The engineer joined your crew!")
       (exit)))))


(setq on-dialog-declined exit)
