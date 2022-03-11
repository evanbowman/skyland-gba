;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-file "/scripts/reset_hooks.lisp")


(if (not (equal (zone) last-zone))
    (progn
      (setq friendlies-seen '())
      (setq enemies-seen '())))


(setq last-zone (zone))


(setq
 next
 (if (and (equal (length enemies-seen) 0) (equal (zone) 0))
     (progn
       (push 'enemies-seen 0)
       (lambda (eval-file "/scripts/event/hostile/0/0.lisp")))
   ;; Sometimes, procedurally generate an enemy. More frequently at lower levels.
   (if (< (choice 100) (get '(50 35 25 10) (zone)))
       (progn
         (push 'enemies-seen -1)
         procgen)
     (eval-file "/scripts/event/hostile_pick_template.lisp"))))


;; Just so we avoid evaluating another file while already evaluating a
;; relatively large expression. Unlikely to be problematic, but as we're running
;; on a small embedded cpu, no need to keep objects alive longer than needed.
(next)

(unbind 'next)

(gc)
