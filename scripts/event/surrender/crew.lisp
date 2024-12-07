
(dialog
 "<c:goblin pirates:2>We surrender! Honesst, we promise not to pillage any other cassstles!")

(defn on-dialog-closed ()
  (setq on-dialog-closed '())
  (let ((c (int (* 0.72 (float (coins-victory))))))
    (dialog "The goblins offer surrender, accept terms?")

    (dialog-opts-reset)
    (dialog-opts-push (format "+1 crew, +%@" c)
                      (lambda ()
                        (coins-add c)
                        (run-util-script
                         "find-crew-slot"
                         "<c:goblin:18> gahh! <B:0> You are out of ssspace! <B:0> NIce hoomansses will not leave us! I'll build an ledarr!"
                         'ladder
                         "Place block (1x2):"
                         (lambda (x y _)
                           (chr-new (player) x y 'neutral '((race . 1) (icon . 18)))
                           (adventure-log-add 51 '())
                           (dialog "One of the goblins joined your crew!")
                           (run-util-script "hostile-pickup-cart"
                                            8
                                            "While scanning the goblin fortress' computers, you find some fascinating images of the surface world. You record them on a cartridge...")
               
                          (exit 2)))))
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

    (dialog-opts-push (format "unacceptable! (+%@)" (coins-victory)) (lambda ()))))

(gc)
