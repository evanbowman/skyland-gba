;;;
;;; util/find-crew-slot.lisp
;;;


(lambda (no-space-text room-sym prompt-text)
  (let ((slots (chr-slots (player))))
    (if slots
        ;; The simple scenario: there's an empty space to place a crewmember.
        (sample slots)
        ;; The complex scenario: there's no room on the island...
        (progn
          (await (dialog* "Sadly, there's no room..."))
          (alloc-space room-sym)
          (await (dialog* no-space-text))
          (let ((xy (await (sel-input* room-sym prompt-text))))
            (sound "build0")
            (room-new (player) (list room-sym (car xy) (cdr xy)))
            xy)))))
