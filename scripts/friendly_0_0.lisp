;;;
;;; friendly_0_0.lisp
;;;


(dialog "In the distance, you see an island inhabited by a lone castaway...")


(init-opponent 5 'neutral)


(configure-player
 (opponent)
 '((power-core 3 13)))


(add-character (opponent) 1 14)


(set 'after-converge
     (lambda
       (dialog "Invite castaway aboard?")

       (await-dialog-y/n)

       (set 'after-converge nil)))


(set 'after-dialog-accepted
     (lambda

       ;; (set 'temp (find-unused-character-slot (player)))
       ;; (if temp
       ;;     (progn
       ;;       (remove-character (opponent) 1 14)
       ;;       (add-character (player) (car temp) (cdr temp)))
       ;;     (dialog "Sadly, there's no room for the castaway in your castle."))

       (dialog "The castaway joined your crew!")

       (set 'after-dialog-accepted nil)
       (set 'after-dialog-declined nil)))


(set 'after-dialog-declined
     (lambda
       (exit-level)
       (set 'after-dialog-accepted nil)
       (set 'after-dialog-declined nil)))
