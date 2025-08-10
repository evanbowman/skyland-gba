;;;
;;; neutral/1/8_0_human.lisp
;;;


(dialog
 "<b:/scripts/data/img/docks.img.bin>While docking at a crowded transit hub, you notice a dog wandering between the platforms... ")


(opponent-init 11 'neutral)


(island-configure
 (opponent)
 '((canvas 0 13 (17 1111359633 58775552 -268370297 20742151 224)) (canvas 0 11 (36 -300285807 17328391 -2005475154 133044864 -1059210399 947392400 -117412336 -721157960 224 15 0 32)) (shrubbery 0 14) (hull 1 14) (canvas 1 13 (31 1111097489 25225216 -2012929886 1344562190 1074003992 -298703852 -1008909180 250 0 40)) (canvas 1 11 (34 1111097489 8972288 50528354 65142928 101976473 84934912 -1581577207 -26200303 128 10)) (hull 2 12) (hull 2 11) (hull 2 13) (hull 2 14) (hull 3 14) (masonry 3 13 3) (masonry 4 14 3) (masonry 4 13 3) (hull 4 12) (hull 4 11) (war-engine 5 11) (hull 6 10) (hull 7 10) (power-core 8 13)))


(chr-new (opponent) 0 12 'neutral '((race . 3) (icon . 24)))
(chr-new (opponent) 1 12 'neutral nil)


(defn on-converge ()
  (dialog
   "<c:Station Master:9>Poor thing's owner had to evacuate ahead of the storm. Been trying to find someone to take him in before we have to close the station...")

  (setq on-dialog-closed
        (lambda ()
          (dialog "He seems friendly, invite him aboard?")
          (dialog-await-y/n)
          (setq on-dialog-closed '())))

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (run-util-script
   "find-crew-slot"
   "<c:Dog:24>BOWOWOWOW!"
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (chr-del (opponent) 0 12)
     (chr-new (player) x y 'neutral '((race . 3) (icon . 24)))
     (dialog "<c:Dog:24>Woof! Bowowow!")
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (dialog "A new friend joins your crew!")
       (defn on-dialog-closed ()
         (dialog "<c:Station Master:9>Not much of a mechanic, but quick on his feet and fierce in a fight!")
         (setq on-dialog-closed exit))))))


(setq on-dialog-declined exit)
