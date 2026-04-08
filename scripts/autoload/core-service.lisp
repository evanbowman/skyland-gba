;;;
;;; autoload/core-service.lisp
;;;

(tr-bind-current)


(lambda (ret)
  (let ((done ret)
        (text nil)
        (skip (list)))

    (setq text (format (tr "<c:Sylph:46>Ah! A sky-dweller with a… % …what is your word… ah yes, 'power-core'! <B:0> Fascinating construction, though the harmonic resonance is quite… % … limited? <B:0> Your crystal vibrates at such a… % …forgive me, your language lacks precision.<B:0> Perhaps I could… % … retune? Yes, retune your matrix to sing in different frequencies? I have studied many resonance patterns from the ancient archives…")
                       "<S:1>harmonic matrix<S:0>"
                       "<S:1>limited<S:0>"
                       "<S:1>pedestrian tone<S:0>"
                       "<S:1>retune<S:0>"))

    (lambda ()

      (when (adv-var-load 'sylph-shop-intro)
        (setq text (tr "<c:Sylph:46>What would you like to do?")))

      (dialog text)
      (adv-var-store 'sylph-shop-intro 1)
      (dialog-opts-reset)

      (dialog-opts-push (tr "Tune core frequency.")
                        (lambda ()
                          (let ((t (this)))
                            (let ((update-color (lambda (cl sp1 sp2 txt)
                                                  (let ((color cl)
                                                        (spr1 sp1)
                                                        (spr2 sp2)
                                                        (text txt))
                                                    (lambda ()
                                                      (setvar "energy_glow_color" color)
                                                      (setvar "spr_energy_color_1" spr1)
                                                      (setvar "spr_energy_color_2" spr2)
                                                      (repaint)
                                                      (if (filter (equalto? color) skip)
                                                          (setq text (tr "Done!"))
                                                          (setq skip (cons color skip)))
                                                      (dialog (tr "<c:Sylph:46>") text)
                                                      (setq on-dialog-closed t))))))

                              (dialog (tr "<c:Sylph:46>Which color?"))
                              (dialog-opts-reset)

                              (map (lambda (info)
                                     (dialog-opts-push (car info)
                                                       (apply update-color (cdr info))))
                                   (eval-file (format "/strings/%/tuning-crystal.lisp"
                                                      (lang))))

                              (dialog-opts-push (tr "Done.") done)))))

      (dialog-opts-push (tr "Buy tuning crystal.")
                        (lambda ()
                          (dialog (tr "<c:Sylph:46>You'd like to buy this tuning equipment? Maybe... <B:0> It'll be a bit pricey, 8000@, but you'll be able to retune your core whenever you want..."))
                          (dialog-opts-reset)
                          (dialog-opts-push (tr "Buy equipment. (8000@)")
                                            (lambda ()
                                              (if (> (coins) 7999)
                                                  (progn
                                                    (coins-set (- (coins) 8000))
                                                    (sel-input 'tuning-crystal
                                                               (tr "place tuning equipment:")
                                                               (lambda (isle x y)
                                                                 (room-new (player) `(tuning-crystal ,x ,y))
                                                                 (sound "build0")
                                                                 (achieve 21))))
                                                  (dialog (tr "<c:Sylph:46>Unfortunately, you don't seem to have enough to buy it yet...")))))

                          (dialog-opts-push (tr "Nevermind.") done)))

      (dialog-opts-push (tr "Nevermind.") done))))
