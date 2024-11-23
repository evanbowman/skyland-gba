;;;
;;; template.lisp
;;;
;;; An example template for creating tutorials. When the (autopilot) function is
;;; not called in a tutorial script, the game allows you to record tutorials
;;; instead. Just add your script to tutorials.lisp, record keypresses, and the
;;; insert them back into this script (see comments below).
;;;

(coins-set 5000)

(terrain-set (player) 4)

(island-configure
 (player)
 '((power-core 1 13)))

(chr-new (player) 1 14 'neutral nil)


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((power-core 1 13)))


(chr-new (opponent) 1 14 'hostile nil)

(dialog "This is a template for a tutorial, see <B:0> /scripts/tutorials/template.lisp for more information.")

;; (autopilot '(<key presses here...>))


(defn tutorial-done ()
  (fatal "see log.txt in save data for key presses..."))

;; Hold start, then press the right bumper to enter shortcut mode. Next press
;; down twice to finish recording. This will trigger writing log.txt to save
;; data, so that you can retrieve the key presses.
(key-bind "dd" 'tutorial-done) ;; bind down-down sequence to function
