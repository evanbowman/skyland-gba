
(dialog
 "<c:goblin pirates:2>We surrender! Honesst, we promise not to pillage any other cassstles!")

(defn on-dialog-closed [0]
    (setq on-dialog-closed '())
  (let ((c (+ (/ (coins-victory) 2) (/ (coins-victory) 6))))
    (dialog "The goblins offer surrender, accept terms?")

    (dialog-opts-reset)
    (dialog-opts-push (format "+1 crew +%@" c)
                      (lambda
                          (coins-add c)
                        (let ((g (chrs (opponent)))
                              (ss (chr-slots (player))))

                          (unless ss
                            (alloc-space 'ladder)

                            (let ((s (construction-sites (player) '(1 . 2))))
                              (room-new (player) (list 'ladder (caar s) (cdr (car s))))
                              (setq ss (chr-slots (player)))))

                          (if g
                              (let ((s (get ss (choice (length ss)))))
                                (chr-del (opponent)
                                         (caar g)
                                         (cdr (car g)))
                                (chr-new (player)
                                         (car s)
                                         (cdr s)
                                         'neutral
                                         '((race . 1)))
                                (adventure-log-add 51 '())
                                (dialog "One of the goblins joined your crew!")
                                (run-util-script "pickup_cart.lisp"
                                                 8
                                                 "While scanning the goblin fortress' computers, you find some fascinating images of the surface world. You record them on a cartridge..."))))
                        (exit 2)))

    (let ((cnt 0)
          (tot (/ (length (rooms (opponent))) 8)))
      (setq cnt tot)
      (when cnt
        (dialog-opts-push
         (format "salvage rights: % blocks" cnt)
         (lambda
           (let ((rtry (this)))
             (sel-input-opponent
              nil
              (format "Take block: (%/%)" (- tot cnt) tot)
              (lambda
                (let ((took (car (room-load (opponent) $1 $2))))
                  (if (room-is-critical (opponent) $1 $2)
                      (progn
                        (dialog "You shouldn't remove the island's only power source! We're accepting their surrender, not trying to sink them!")
                        (if (equal 1 (length (rooms (opponent))))
                            (exit)
                            (setq on-dialog-closed rtry)))
                      (progn
                        (room-del (opponent) $1 $2)
                        (sound "gravel")
                        (alloc-space took)
                        (sel-input
                         took
                         (format "Place block: (%/%)" (- tot cnt) tot)
                         (lambda
                           (room-new (player) (list took $1 $2))
                           (sound "build0")
                           (setq cnt (- cnt 1))
                           (if (equal cnt 0)
                               (progn
                                 (dialog (format "Accepted surrender and gained % blocks!" tot))
                                 (adventure-log-add 62 '())
                                 (setq on-dialog-closed exit))
                               (rtry))))))))))))))


    (let ((rtry (this)))
      (dialog-opts-push "(help me decide!)"
                        (lambda
                            (dialog
                             "<s:3>. . . . . <s:0> "
                             "Your crew values the resources in the goblin ship at "
                             (format "%@" (coins-victory))
                             ". <B:0>Or you may end the fight and accept the terms of the "
                             "goblins' surrender...")

                          (setq on-dialog-closed rtry))))


    (dialog-opts-push "unacceptable!" (lambda nil))))

(gc)
