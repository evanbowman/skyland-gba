;;;
;;; reset_hooks.lisp
;;;


;; Callbacks invoked by the engine for various events.
(setq on-fadein '())           ; After fadein at beginning of level.
(setq on-converge '())         ; When other island finishes approaching player.
(setq on-dialog-accepted '())  ; Upon selecting yes in a dialog prompt.
(setq on-dialog-declined '())  ; Upon selecting no in a dialog prompt.
(setq on-dialog-closed '())    ; Dialog box closed.
(setq on-victory '())          ; Defeated opponent island.
(setq on-room-destroyed '())   ; A block was destroyed. [island, type, x, y]
(setq on-crew-died '())        ; Crewmember died. Parameters: [chr-id]
(setq on-shop-item-sel '())    ; Shop item selected



(if (not (bound 'last-zone))
    (setq last-zone (zone)))


(if (not (bound 'enemies-seen))
    (setq enemies-seen '()))


(if (not (bound 'friendlies-seen))
    (setq friendlies-seen '()))


(gc)
