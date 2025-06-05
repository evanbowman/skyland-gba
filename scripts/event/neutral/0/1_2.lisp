;;;
;;; neutral/0/1.lisp
;;;


(dialog
 "<b:/scripts/data/img/toll_station.img.bin> "
 "An ancient imperial missile platform contacts you and demands payment...")


(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((hull 0 12)
   (arc-gun 0 10)
   (hull 0 13)
   (hull 0 8)
   (hull 0 9)
   (hull 0 7)
   (hull 0 11)
   (power-core 1 11)
   (hull 1 14)
   (hull 1 7)
   (hull 1 8)
   (power-core 1 9)
   (hull 1 13)
   (hull 2 14)
   (missile-silo 2 7)
   (hull 2 13)
   (hull 3 7)
   (hull 3 8)
   (hull 3 9)
   (hull 3 10)
   (hull 3 11)
   (hull 3 12)
   (hull 3 13)
   (rocket-bomb 4 8)
   (hull 4 11)))

(flag-show (opponent) 4)


(defn on-converge ()
  (dialog
   "While the old empire is now fragmented and most of its weapons systems are offline, "
   "this automated vessel seems to still be functioning. <B:0>"
   "The station's computers demand a toll of 600@. Pay?")
  (dialog-opts-reset)
  (dialog-await-y/n)
  (setq on-converge nil))


(let ((scr
       (lambda (txt)
         (dialog txt)
         (opponent-mode 'hostile))))
  (setq on-dialog-accepted
        (lambda ()
          (if (< (coins) 600)
              (progn
                (adventure-log-add 59 '())
                (scr "Insufficient funds! The station begins charging its weapons!"))
            (progn
              (adventure-log-add 60 (list 600))
              (coins-add -600)
              (dialog "The station deactivates and allows you to pass.")
              (exit)))))


  (setq on-dialog-declined
        (lambda ()
          (adventure-log-add 61 '())
          (scr "The station begins charging its weapons!"))))
