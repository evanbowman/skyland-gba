;;;
;;; globals.lisp
;;;
;;; The interpreter does not allow you to set a variable that isn't either a let
;;; binding or define explicitly as global.
;;;


(global
 'on-fadein
 'on-converge
 'on-dialog-closed
 'on-victory
 'on-room-destroyed
 'on-crew-died
 'on-shop-item-sel
 'on-shop-enter
 'on-dialog-accepted
 'on-dialog-declined
 'on-level-exit
 'on-menu-resp
 'last-zone
 'enemies-seen
 'friendlies-seen
 'surprises-seen
 'qids
 'quests
 'adventure-log
 'shop-items
 'zone-shop-items
 'qvar
 'pending-events
 'debrief-strs
 'adv-var-set
 'adv-var-list)


(defconstant difficulty-beginner 0)
(defconstant difficulty-normal 1)
(defconstant difficulty-hard 2)
(defconstant quest-count 9)
(defconstant surprise-count 3)

(defconstant weather-id-clear 1)
(defconstant weather-id-sunshower 2)
(defconstant weather-id-rain 3)
(defconstant weather-id-snow 4)
(defconstant weather-id-storm 5)
(defconstant weather-id-ash 6)
(defconstant weather-id-night 7)
(defconstant weather-id-solar-storm 8)

(defconstant flag-id-pirate 0)
(defconstant flag-id-marauder 1)
(defconstant flag-id-old-empire 4)
(defconstant flag-id-banana 5)
(defconstant flag-id-merchant 6)
(defconstant flag-id-colonist 7)
(defconstant flag-id-sylph 36)

(defconstant faction-enable-human-mask (bit-shift-left 1 0))
(defconstant faction-enable-goblin-mask (bit-shift-left 1 1))
(defconstant faction-enable-sylph-mask (bit-shift-left 1 2))
