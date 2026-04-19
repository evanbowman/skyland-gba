;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios.
;;;


(eval-file "/scripts/reset_hooks.lisp")
(eval-file "/scripts/event/check_zone.lisp")


(defn/temp process-pending-events ()
  (let ((event-fired nil))
    (setq pending-events
          (map (lambda (ev)
                 (let ((steps (car ev))
                       (event (cdr ev)))
                   (setq steps (- steps 1))
                   (when (< steps 1)
                     (setq event-fired event))
                   (cons steps event)))
               pending-events))
    (setq pending-events
          (filter (lambda (ev)
                    (not (equal (cdr ev) event-fired)))
                  pending-events))
    event-fired))



(let ((event (process-pending-events)))
  (cond
    (event
     (eval-file event))
    ((and (equal (length enemies-seen) 0) (equal (zone) 0))
     (push enemies-seen 0)
     (eval-file "/scripts/event/hostile/0/0.lisp"))
    ((< (choice 100) (get '(60 55 50 45) (zone)))
     (push enemies-seen -1)
     (procgen))
    (true
     ((eval-file "/scripts/event/hostile_pick_template.lisp")))))


(defn/temp collect-chrs ()
  (map (lambda (c) (lookup 'id (cddr c)))
       (chrs (player))))


(let ((vfn on-victory) ;; Save cached copy of on-victory hook in case already set.
      (c (coins))
      (prev-crew (collect-chrs)))
  (defn on-victory ()

    (let* ((current-crew (collect-chrs))
           (crew-died (filter (lambda (id) (not (contains current-crew id))) prev-crew)))
      (when crew-died
        (adventure-log-add 4 (list (length crew-died)))))

    (adventure-log-add 2 (list (- c (coins)) (coins-victory)))

    (when vfn
      (vfn))))
