;;;
;;; neutral/2/6.lisp
;;;


(dialog "An acrid plume of smoke rises from a fortress on the horizon...")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((incinerator 0 13)
   (hull 2 14)
   (power-core 3 13)
   (hull 3 12)
   (hull 5 14)
   (torch 2 10)
   (torch 3 9)
   (torch 5 11)))

(defn on-fadein
  (map (lambda
         (if (equal (get $0 0) 'torch)
             (fire-new (opponent) (get $0 1) (get $0 2))))
       (rooms (opponent)))
  (setq on-fadein nil))

;; (flag-show (opponent) 1)


(defn on-converge
  (dialog
   "Looks like a terrible battle happened here... the crew seems to have abandoned the burning island, leaving behind a powerful weapon...")
  (setq on-converge nil)
  (while (not (construction-sites (player) '(2 . 2)))
    (terrain (player) (+ terrain (player) 1)))
  (sel-input 'incinerator
             "Place weapon (2x2)"
             (lambda
               (room-new (player) (list 'incinerator $1 $2))
                 (room-del (opponent) 0 13)
               (syscall "sound" "build0")
               (dialog "A delicate weapon built long ago on the surface... protect it carefully, because you can't build a replacement.")
               (setq on-dialog-closed exit))))
