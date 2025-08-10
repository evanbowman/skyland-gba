;;;
;;; neutral/2/6_4.lisp
;;;


(dialog "Your castle sails through a dense bank of clouds, and when the mist finally clears... <B:0> <b:/scripts/data/img/sylph_archive.img.bin> An impossible sight emerges - massive walls of ancient stonework stretch upward as far as you can see, flanking a narrow chasm that seems to have no bottom. <B:0> Your island drifts forward, a tiny speck between towering structures of ornate masonry and bronze. <B:0> A sudden voice crackles over your radio, precise and formal...")


(opponent-init 4 'hostile)
(island-configure (opponent) '((power-core 1 13)))


(defn on-fadein ()
  (dialog "<c:Sylph Archivist:21>Greetings, traveller. Few outsiders reach the Central Archive of the Sylph Conclave. <B:0> We have observed your fortress with great interest. Your configuration of technology, your adaptations to the changing skies... they represent solutions we would not have designed. Our archive seeks to document all knowledge of sky survival. Your castle's design holds valuable insights for our studies. <B:0> We propose an exchange. Allow us to keep your current fortress for our archives. In return, we offer one of our own warships. Your crew would transfer completely. Nothing would be lost but the structure itself...")
  (dialog-await-binary-q "Accept the exchange." "Decline and leave."))


(defn on-dialog-accepted ()
  (let ((chr-list (chrs (player)))
        (rem-list nil))

    (island-configure
     (player)
     '((bronze-hull 0 9) (power-core 0 10) (phase-shifter 0 12) (reactor 1 12) (bronze-hull 1 8) (bronze-hull 1 9) (bronze-hull 2 9) (workshop 2 10) (bronze-hull 2 8) (forcefield 3 7) (reactor 3 12) (missile-silo 3 8) (bulkhead-door 4 10) (bronze-hull 4 9) (bronze-hull 4 8) (ballista 5 12) (bronze-hull 5 10) (bronze-hull 5 9) (portal 5 14) (amplifier 5 13) (portal 5 11) (sylph-cannon 6 13) (sylph-cannon 6 14) (bronze-hull 6 10) (deflector 6 11) (forcefield 8 14) (forcefield* 8 13) (forcefield 8 12) (forcefield* 8 11) (forcefield 8 10)))

    (terrain-set (player) 9)

    (while chr-list
      (let ((slots (chr-slots (player))))
        (if slots
            (let ((slot (car slots)))
              (chr-new (player) (car slot) (cdr slot) 'neutral (cdr (cdr (car chr-list)))))
            (setq rem-list (cons (car chr-list) rem-list)))
        (setq chr-list (cdr chr-list))))

    (while rem-list
      (let ((sl (chr-slots (player))))
        (when (not sl)
          (terrain-add-left (player))
          (room-new (player) '(stairwell 0 11))
          (setq sl (chr-slots (player))))
        (chr-new (player) (caar sl) (cdar sl) 'neutral (cdr (cdr (car rem-list))))
        (setq rem-list (cdr rem-list))))

    (dialog "<c:Sylph Archivist:21>A wise decision. Your fortress will be preserved in our archives, studied and honored. <B:0> May the winds favor your journey!")
    (setq on-dialog-closed exit)))


(defn on-dialog-declined ()
  (dialog "<c:Sylph Archivist:21>We understand your attachment to your fortress. It is a creation you have shaped with your own hands and vision.")
  (exit))
