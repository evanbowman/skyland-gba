
(lambda (ret)
  (let ((done ret))
    (lambda ()
      (let ((weapon-damage 0)
            (hull-damage 0))
        (foreach (lambda (room)
                   (let ((damage (room-damage (player) (get room 1) (get room 2))))
                     (if damage
                         (let ((cg (rinfo 'category (car room))))
                           (cond
                             ((equal cg 'wall)
                              (+= hull-damage damage))
                             ((equal cg 'weapon)
                              (+= weapon-damage damage)))))))
                 (rooms (player)))

        (dialog "<c:repairman:30> What a nice ship you have here... <B:0> "
                "GAH! It's got dents all over it! <B:0>"
                "Let's see... <B:0>"
                (if weapon-damage
                    (format "%@ to fix the damage to your weapons... <B:0>"
                            (* 3 weapon-damage))
                    "")
                (if hull-damage
                    (format "%@ to fix that hull damage... <B:0>"
                            (int (* 0.75 (float hull-damage))))
                    "")
                "And 200@ for the repair fee. <B:0>"
                "Want to go ahead with the repairs?")

        (dialog-opts-reset)

        (let ((cost (+ (* 3 weapon-damage)
                       (int (* 0.75 (float hull-damage)))
                       200)))
          (dialog-opts-push (format "Yes! (%@)" cost)
                            (lambda ()
                              (if (< (coins) cost)
                                  (progn
                                    (dialog "<c:repairman:30> Sorry, you can't afford it!")
                                    (setq on-dialog-closed done))
                                  (progn
                                    (sound "build0.raw")
                                    (coins-set (- (coins) cost))
                                    (foreach (lambda (room)
                                               (room-hp-set (player)
                                                            (get room 1)
                                                            (get room 2)
                                                            (rinfo 'max-hp (car room))))
                                             (rooms (player)))
                                    (dialog "<c:repairman:30> <s:3>. . . . . <s:0> "
                                            "OK! All finished! <B:0> Nice working with ya!")
                                    (setq on-dialog-closed done)))))

          (dialog-opts-push "nevermind..."
                            (lambda ()
                              (dialog "<c:repairman:30> No problem! Have a nice day!")
                              (setq on-dialog-closed done))))))))
