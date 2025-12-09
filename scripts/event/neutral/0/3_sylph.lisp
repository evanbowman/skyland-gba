;;;
;;; neutral/0/3_sylph.lisp
;;;


(dialog "A Sylph supply depot appears through the clouds, its crystalline sensors tracking your approach. <B:0> Your radio crackles with a precise, emotionless transmission...")

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))


(chr-new (opponent) 1 14 'neutral '((race . 4)))
(chr-new (opponent) 2 14 'neutral '((race . 4)))


(flag-show (opponent) 36) ;; Sylph flag


(defn/temp place-items (variant resp)
  (let ((reply resp)
        (item variant))
    (lambda ()
      (alloc-space item)
      (sel-input
       item
       (string
        "Install first "
        (rinfo 'name item)
        (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
       (lambda (isle x y)
         (room-new (player) (list item x y))
         (sound "build0")
         (alloc-space item)
         (sel-input
          item
          (string "Install second " (rinfo 'name item) ":")
          (lambda (isle x y)
            (room-new (player) (list item x y))
            (sound "build0")
            (dialog reply)
            (setq on-dialog-closed exit))))))))


(let ((item (sample '(arc-gun flak-gun beam-gun)))
      (skip 1))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))


  (setq on-converge
        (lambda ()
          (dialog "<c:Sylph Quartermaster:53>Scanning vessel configuration... <B:0> Analysis: sub-optimal armament for projected threat parameters in your operational zone. <B:0> Conclave logistics has authorized requisition of two "
                  (rinfo 'name item)
                  " emplacements to your vessel. <B:0> Cost: materials only, 1300@."
                  (if (< (coins) 1300)
                      " <B:0> ...insufficient funds detected. I will maintain station for 15 seconds while you adjust resource allocation."
                      ""))
          (dialog-opts-reset)
          (dialog-opts-push "Accept for 1300@." on-dialog-accepted)
          (dialog-opts-push "Refuse allocation."
                            (lambda ()
                              (dialog "<c:Sylph Quartermaster:53>Acknowledged. Tactical assessment indicates 73% probability you will not survive the next three hostile encounters with current armament. <B:0> Your decision is... inefficient. <B:0> Requisition cancelled.")
                              (setq on-dialog-closed exit)))
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
                          (dialog "<c:Sylph Quartermaster:53>Resources now sufficient. Proceeding with transfer.")
                          (setq on-dialog-closed f))
                        (f))))

                (if skip
                    (progn
                      (setq skip 0)
                      (on-timeout 15000 'fut))
                    (progn
                      (dialog "<c:Sylph Quartermaster:53>Resources still insufficient. Do you require additional time to salvage equipment? I will check status again in 15 seconds.")
                      (dialog-await-y/n)
                      (setq on-dialog-accepted (lambda () (on-timeout 15000 'fut)))
                      (setq on-dialog-declined (lambda () (unbind 'fut) (exit))))))
              (progn
                (adventure-log-add 67 (list (rinfo 'name item) 1300))
                (coins-add -1300)
                ((place-items item "<c:Sylph Quartermaster:53>Transfer complete. Conclave tactical projections indicate improved survival probability. <B:0> Maintain operational readiness. Quartermaster out.")))))))

(setq on-dialog-declined exit)
