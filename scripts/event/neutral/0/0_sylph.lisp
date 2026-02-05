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
          (dialog-setup-binary-q-w/lore "Welcome aboard!" "Not today."
                                        '(("crystal deterioration?" .
                                           "<c:Sylph Engineer:48>The crystals were harvested from our mountain homes, back before the migration. <B:0> They worked perfectly for centuries. But something in the upper atmosphere is breaking them down - micro-fractures spreading through the matrix. <B:0> We don't have access to the source veins anymore. Can't grow new ones. <B:0> Every city is rationing power now, trying to make the old crystals last.")
                                          ("the goblins?" .
                                           "<c:Sylph Engineer:48>They showed up about a day after my core failed. Prowling around, testing for weak spots. <B:0> I shut down all systems, made it look abandoned. They poked around for a while, took some supplies, then moved on. <B:0> They're getting bolder. They know we're vulnerable now.")))
          (setq on-dialog-closed '())))

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (find-crew-slot-cb
   "<c:Sylph Engineer:48>Ah. Full capacity. Expected, really. <B:0> Let me help - I'm quite good at spatial optimization. Professional habit."
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (adventure-log-add 7 '())
     (chr-del (opponent) 1 14)
     (chr-new (player) x y 'neutral '((race . 4) (icon . 48)))
     (dialog "<c:Sylph Engineer:48>Hm. Your core... <B:0> It's human make, yes? Crude by our standards, but... <B:0> <d:800> ...it's working. That's more than I can say for mine. <B:0> I'll keep it running well. <B:0> <d:600> It feels good to have systems to maintain again.")
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (dialog "The engineer joined your crew!")
       (exit)))))


(setq on-dialog-declined exit)
