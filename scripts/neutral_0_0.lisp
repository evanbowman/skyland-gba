;;;
;;; neutral_0_0.lisp
;;;


(dialog "In the distance, you see an island inhabited by a lone castaway...")


(init-opponent 5 'neutral)


(configure-player
 (opponent)
 '((power-core 3 13)))


(add-chr (opponent) 1 14 'neutral 0)


(set 'after-converge-hook
     (lambda
       (dialog "Invite castaway aboard?")

       (await-dialog-y/n)
       (set 'after-converge-hook nil)))


(set 'after-dialog-accepted-hook
     (lambda

       (set 'temp (chr-slots (player)))

       (if temp
           (progn
             (set 'temp (get temp (cr-choice (length temp))))
             (add-chr (player) (car temp) (cdr temp) 'neutral 0)
             (rem-chr (opponent) 1 14)
             (set 'temp '())
             (dialog "The castaway joined your crew!"))
         (dialog "Sadly, there's no room..."))

       (exit-level)))


(set 'after-dialog-declined-hook
     (lambda
       ;; TODO...
       (exit-level)))
