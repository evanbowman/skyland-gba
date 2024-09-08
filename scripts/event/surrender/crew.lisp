
(dialog
 "<c:goblin pirates:2>We surrender! Honesst, we promise not to pillage any other cassstles!")

(defn on-dialog-closed ()
    (setq on-dialog-closed '())
  (let ((c (+ (/ (coins-victory) 2) (/ (coins-victory) 6))))
    (dialog "The goblins offer surrender, accept terms?")

    (dialog-opts-reset)
    (dialog-opts-push (format "+1 crew, +%@" c)
                      (lambda ()
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
                                (run-util-script "hostile_pickup_cart.lisp"
                                                 8
                                                 "While scanning the goblin fortress' computers, you find some fascinating images of the surface world. You record them on a cartridge..."))))
                        (exit 2)))

    (let ((cnt 0)
          (tot (/ (length (rooms (opponent))) 5)))
      (setq cnt tot)
      (when cnt
        (dialog-opts-push
         (format "salvage rights: % blocks" cnt)
         (lambda ()
           (let ((rtry (this)))
             (sel-input-opponent
              nil
              (format "Take block: (%/%)" (- tot cnt) tot)
              (lambda (isle x y)
                (let ((took (car (room-load (opponent) x y))))
                  (if (room-is-critical (opponent) x y)
                      (progn
                        (dialog "This will remove the island's only power source, causing it to become unstable (you won't be able to take any more blocks), are you sure?")
                        (dialog-await-y/n)
                        (setq on-dialog-declined rtry)
                        (let ((tx x)
                              (ty y))
                          (defn on-dialog-accepted ()
                            (sound "gravel")
                            (alloc-space took)
                            (sel-input
                             took
                             "Place block:"
                             (lambda (isle x y)
                               (room-new (player) (list took x y))
                               (sound "build0")
                               (room-del (opponent) tx ty)
                               (dialog (format "Accepted surrender, and acquired % blocks!" (- tot (- cnt 1))))
                               (adventure-log-add 62 '())
                               (setq on-dialog-closed (curry exit 2)))))))
                      (progn
                        (room-del (opponent) x y)
                        (sound "gravel")
                        (alloc-space took)
                        (sel-input
                         took
                         (format "Place block: (%/%)" (- tot cnt) tot)
                         (lambda (isle x y)
                           (room-new (player) (list took x y))
                           (sound "build0")
                           (setq cnt (- cnt 1))
                           (if (equal cnt 0)
                               (progn
                                 (dialog (format "Accepted surrender, and acquired % blocks!" tot))
                                 (adventure-log-add 62 '())
                                 (setq on-dialog-closed (curry exit 2)))
                               (rtry))))))))))))))


    (let ((rtry (this)))
      (dialog-opts-push "(help me decide!)"
                        (lambda ()
                            (dialog
                             "<s:3>. . . . . <s:0> "
                             "Your crew values the resources in the goblin ship at "
                             (format "%@" (coins-victory))
                             ". <B:0>Or you may end the fight and accept the terms of the "
                             "goblins' surrender. <B:0>"
                             "Goblins may offer you crewmembers, blocks, and/or resources (@) in their surrender options.")

                          (setq on-dialog-closed rtry))))


    (dialog-opts-push "unacceptable!" (lambda ()))))

(gc)
