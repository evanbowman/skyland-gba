;;;
;;; neutral_0_0.lisp
;;;


(dialog "In the distance, you see an island inhabited by a lone castaway...")


(init-opponent 5 'neutral)


(configure-player
 (opponent)
 '((power-core 3 13)))


(add-chr (opponent) 1 14 'neutral 0)


(def after-converge-hook
     (lambda
       (dialog "Invite castaway aboard?")

       (await-dialog-y/n)
       (def after-converge-hook nil)))


(def after-dialog-accepted-hook
     (lambda

       (def temp (chr-slots (player)))

       (if temp
           (progn
             (def temp (get temp (choice (length temp))))
             (add-chr (player) (car temp) (cdr temp) 'neutral 0)
             (rem-chr (opponent) 1 14)
             (def temp '())
             (dialog "The castaway joined your crew!"))
         (dialog "Sadly, there's no room..."))

       (exit-level)))


(def after-dialog-declined-hook
     (lambda
       ;; TODO...
       (exit-level)))
