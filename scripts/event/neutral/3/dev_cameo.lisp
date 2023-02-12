;;;
;;; neutral/3/dev_cameo.lisp
;;;


(dialog "A precarious-looking fort drifts out of the clouds...")


(opponent-init 7 'neutral)


(island-configure
 (opponent)
 '((masonry 0 14 0)
   (bronze-hull 0 13)
   (water-source 1 14)
   (power-core 2 7)
   (masonry 2 14 0)
   (masonry 2 13 0)
   (workshop 2 11)
   (masonry 3 14 0)
   (masonry 3 13 0)
   (workshop 3 9)
   (water-source 4 14)
   (water-source 5 14)
   (masonry 6 14 0)
   (bronze-hull 6 13)))


(setq on-converge
      (lambda
        (let ((info (cart-info 0)))
          (dialog "Looks like no one's home! The sign on the door reads: 'Evan's hut'. Amongst the clutter, you find a data cartridge!")
          (setq on-dialog-closed
                (lambda
                  (syscall "sound" "click_digital_1")
                  (cart-add 0)
                  (dialog "You pick up a cart labled "
                          (get info 0)
                          "! (To load data carts, go to the extras room of the title screen!)")
                  (setq on-dialog-closed nil)
                  (exit))))))
