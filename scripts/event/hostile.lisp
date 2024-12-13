;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-file "/scripts/reset_hooks.lisp")
(eval-file "/scripts/event/check_zone.lisp")



(if (and (equal (length enemies-seen) 0) (equal (zone) 0))
    (progn
      (push 'enemies-seen 0)
      (eval-file "/scripts/event/hostile/0/0.lisp"))
  ;; Sometimes, procedurally generate an enemy. More frequently at lower levels.
  (if (< (choice 100) (get '(60 55 50 45) (zone)))
      (progn
        (push 'enemies-seen -1)
        (procgen))
    ((eval-file "/scripts/event/hostile_pick_template.lisp"))))


(let ((vfn on-victory) ; save cached copy of on-victory hook in case already set
      (c (coins))
      (crew (length (chrs (player)))))
  (defn on-victory ()

    ;; For each crew member at the start of the level, check if crewmember still
    ;; exists. if not, record death in the adventure log.
    (let ((rem (length (chrs (player)))))
      (when (< rem crew)
        (adventure-log-add 4 (list (- crew rem)))))

    (adventure-log-add 2 (list (- c (coins)) (coins-victory)))

    (when vfn
      (vfn))))
