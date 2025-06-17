;;;
;;; neutral/0/3_goblin.lisp
;;;


(dialog "Your sensors detect some goblin scavengers returning from a raid. <B:0> Their leader excitedly waves you down, eager to trade their mysterious finds...")

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))


(chr-new (opponent) 1 14 'neutral '((race . 1)))
(chr-new (opponent) 2 14 'neutral '((race . 1)))



(flag-show (opponent) 1)


(defn/temp place-items (variant resp)
  (let ((reply resp)
        (item variant))
    (lambda ()
      (alloc-space item)
      (sel-input
       item
       (string
        "place first "
        (rinfo 'name item)
        (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
       (lambda (isle x y)
         (room-new (player) (list item x y))
         (sound "build0")
         (alloc-space item)
         (sel-input
          item
          (string "place second " (rinfo 'name item) ":")
          (lambda (isle x y)
            (room-new (player) (list item x y))
            (sound "build0")
            (dialog reply)
            (setq on-dialog-closed exit))))))))


(let ((item (sample '(arc-gun flak-gun fire-charge)))
      (skip 1))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))


  (setq on-converge
        (lambda ()
          (dialog "<c:scavenger:35> Found thessse "
                  (rinfo 'name item)
                  "s"
                  (if (equal (faction) 'goblin)
                      " on a human issle!"
                      "... err ... Well, don't worry where I got them! <B:0>")
                  " Still working, barely sssinged! 1300@ for two, yesss? Better price than waiting for your workshop to build them!"
                  (if (< (coins) 1300)
                      "...but you don't ssseem to have enough. Do you want to ssalvage some stuff to come up with the funds? I'll check back in 15 seconds?"
                      ""))
          (dialog-opts-reset)
          (dialog-opts-push "purchase for 1300@" on-dialog-accepted)
          (dialog-opts-push "take by force"
                            (lambda ()
                              (adventure-log-add 71 (list (rinfo 'name item)))
                              (dialog "<c:scavenger:35>Gah! Fine, take them! <B:0> Ssstolen from some fat merchantsss anyway... <B:0> But we won't forget thisss. We know where to find more friendsss...")
                              (push-pending-event (+ 2 (choice 4)) "/scripts/event/hostile/scavenger-vengence.lisp")
                              (setq on-dialog-closed
                                    (place-items item "The goblins storm off, swearing vengence..."))))
          (dialog-opts-push "decline offer" on-dialog-declined)
          (setq on-converge nil)))



  (setq on-dialog-accepted
        (lambda ()
          (if (bound? 'fut) (unbind 'fut))

          (if (< (coins) 1300)
              (progn
                ;; Capture the current executing function, reinvoke after n seconds...
                (let ((f (this)))
                  (defn fut ()
                    (if (> (coins) 1299)
                        (progn
                          (dialog "<c:scavenger:35> Seems like you have enough now!")
                          (setq on-dialog-closed f))
                        (f))))

                (if skip
                    (progn
                      (setq skip 0)
                      (on-timeout 15000 'fut))
                    (progn
                      (dialog "<c:scavenger:35> Sorry, that's not enough! Do you want to salvage some ssstuff to come up with the ressourcesss for payment? I'll check back in in 15 seconds?")
                      (dialog-await-y/n)
                      (setq on-dialog-accepted (lambda () (on-timeout 15000 'fut)))
                      (setq on-dialog-declined (lambda () (unbind 'fut) (exit))))))
              (progn
                (adventure-log-add 67 (list (rinfo 'name item) 1300))
                (coins-add -1300)
                ((place-items item "<c:scavenger:35> Yesss! Ssmart choice! Don't mind the burn marksss, they add character!")))))))

(setq on-dialog-declined exit)
