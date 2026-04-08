;;;
;;; surrender/crew.lisp
;;;

(tr-bind-current)

(dialog
 (cond
  ((equal (faction) 'goblin)
   (tr "<c:Goblin Pirates:2>We sssurrender! Honessst, we promise to help you pillage any other cassstles!"))
  (true
   (tr "<c:Goblin Pirates:2>We sssurrender! Honessst, we promise not to pillage any other cassstles!"))))

(defn on-dialog-closed ()
  (setq on-dialog-closed '())
  (let ((c (int (* 72/100 (coins-victory))))
        (score-bonus (int (* 1/4 (coins-victory)))))
    (dialog (tr "The goblins offer surrender, accept terms?"))

    (dialog-opts-reset)
    (dialog-opts-push (format (tr "+1 crew, +%@") c)
                      (lambda ()
                        (coins-add c)
                        (score-add score-bonus)
                        (find-crew-slot-cb
                        (if (equal (faction) 'goblin)
                            (tr "<c:Goblin:18>Gahh! <B:0> You are out of ssspace! <B:0> Nice friendsss will not leave usss! I'll build an ledarr!")
                            (tr "<c:Goblin:18>Gahh! <B:0> You are out of ssspace! <B:0> Nice humansss will not leave usss! I'll build an ledarr!"))
                         'ladder
                         (tr "Place block (1x2):")
                         (lambda (x y _)
                           (chr-new (player)
                                    x
                                    y
                                    'neutral
                                    (list (cons 'race 1)
                                          (cons 'icon (sample '(18 35 36 37 41)))))
                           (adventure-log-add 51 '())
                           (dialog (tr "One of the goblins joined your crew!"))
                           (hostile-pickup-cart 8
                                                (tr "While scanning the goblin fortress' computers, you find some fascinating images of the surface world. You record them on a cartridge...")
                                                exit)

                          (exit 2)))))
  (let ((cnt 0)
        (tot (floor (/ (length (rooms (opponent))) 5))))
      (setq cnt tot)
      (when cnt
        (dialog-opts-push
         (format (tr "Salvage rights: % blocks.") cnt)
         (lambda ()
           (let ((rtry (this)))
             (sel-input-opponent
              nil
              (format (tr "Take block: (%/%)") (- tot cnt) tot)
              (lambda (isle x y)
                (let ((took (car (room-load (opponent) x y))))
                  (if (room-is-critical (opponent) x y)
                      (progn
                        (dialog (tr "This will remove the island's only power source, causing it to become unstable (you won't be able to take any more blocks), are you sure?"))
                        (dialog-setup-y/n)
                        (setq on-dialog-declined rtry)
                        (let ((tx x)
                              (ty y))
                          (defn on-dialog-accepted ()
                            (sound "gravel")
                            (alloc-space took)
                            (sel-input
                             took
                             (tr "Place block:")
                             (lambda (isle x y)
                               (room-new (player) (list took x y))
                               (sound "build0")
                               (room-del (opponent) tx ty)
                               (dialog (format (tr "Accepted surrender, and acquired % blocks!") (- tot (- cnt 1))))
                               (adventure-log-add 62 '())
                               (setq on-dialog-closed (curry exit 2)))))))
                      (progn
                        (if (not took)
                            (progn
                              (sound "beep_error")
                              (rtry))
                            (progn
                              (room-del (opponent) x y)
                              (sound "gravel")
                              (alloc-space took)
                              (sel-input
                               took
                               (format (tr "Place block: (%/%)") (- tot cnt) tot)
                               (lambda (isle x y)
                                 (room-new (player) (list took x y))
                                 (sound "build0")
                                 (setq cnt (- cnt 1))
                                 (if (equal cnt 0)
                                     (progn
                                       (dialog (format (tr "Accepted surrender, and acquired % blocks!") tot))
                                       (adventure-log-add 62 '())
                                       (setq on-dialog-closed (curry exit 2)))
                                     (rtry))))))))))))))))


    (dialog-opts-push (format (tr "Unacceptable! (+%@)") (coins-victory)) (lambda ()))))
