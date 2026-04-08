;;;
;;; autoload/pickup-cart.lisp
;;;

(tr-bind-current)

;; args: (cart-id dialog-string)
(lambda (cart text)
  (when (not (cart-found? cart))
    (await (dialog* text))
    (sound "click_digital_1")
    (cart-add cart)
    (await (dialog* (tr "You picked up a cart labeled ")
                    (car (cart-info cart))
                    (format (tr "! (cartridge %)") (+ cart 1))))
    (when (not (save-bit-load 8))
      (await (dialog* (tr "(To load data carts, go to the extras room of the title screen!)")))
      (save-bit-store 8 1))))
