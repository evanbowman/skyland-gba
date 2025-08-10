;;;
;;; util/find-crew-slot.lisp
;;;


(lambda (no-space-text room-sym prompt-text callback)
  (let ((slots (chr-slots (player))))
    (if slots
        ;; The simple scenario: there's an empty space to place a crewmember.
        (let ((slot (sample slots)))
          (callback (car slot) (cdr slot) false))
        ;; The complex scenario: there's no room on the island...
        (progn
          (dialog "Sadly, there's no room...")
          (alloc-space room-sym)
          (let ((cb callback)
                (rsym room-sym)
                (txt1 no-space-text)
                (txt2 prompt-text))
            (defn on-dialog-closed ()
              (dialog txt1)
              (defn on-dialog-closed ()
                (setq on-dialog-closed nil)
                (sel-input rsym
                           txt2
                           (lambda (isle x y)
                             (sound "build0")
                             (room-new (player) (list rsym x y))
                             (cb x y true))))))))))
