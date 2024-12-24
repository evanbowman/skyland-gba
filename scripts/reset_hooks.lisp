;;;
;;; reset_hooks.lisp
;;;


;; Callbacks invoked by the engine for various events.
(setq on-fadein '())           ; After fadein at beginning of level.
(setq on-converge '())         ; When other island finishes approaching player.
(setq on-dialog-closed '())    ; Dialog box closed.
(setq on-victory '())          ; Defeated opponent island.
(setq on-room-destroyed '())   ; A block was destroyed. [island, type, x, y]
(setq on-crew-died '())        ; Crewmember died. Parameters: [chr-id]
(setq on-shop-item-sel '())    ; Shop item selected
(setq on-shop-enter '())       ; Shop entry dialog
(setq on-level-exit '())       ; Hook invoked when leaving a level
(setq on-menu-resp '())        ; Used by some menus via push-menu function
(dialog-opts-reset)

(destroy-temps)

;; legacy callbacks, no longer invoked by the engine, but still used in various
;; scripts.
(setq on-dialog-accepted '())  ; Upon selecting yes in a dialog prompt.
(setq on-dialog-declined '())  ; Upon selecting no in a dialog prompt.


(if (not (bound? 'last-zone))
    (setq last-zone (zone)))


(if (not (bound? 'enemies-seen))
    (setq enemies-seen '()))


(if (not (bound? 'friendlies-seen))
    (setq friendlies-seen '()))

(setvar "rewind_disabled" 0)


;; I try not to run the gc manually. But we just detached a bunch of callbacks
;; that were storing potentially large functions. The collector possibly going
;; to end up needing to run anyway, and I don't want that to happen in the
;; middle of a level, when the pause would be more noticeable.
(gc)
