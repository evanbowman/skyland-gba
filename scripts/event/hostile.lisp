;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-file "/scripts/reset_hooks.lisp")
(eval-file "/scripts/event/check_zone.lisp")


(setq
 next
 (if (and (equal (length enemies-seen) 0) (equal (zone) 0))
     (progn
       (push 'enemies-seen 0)
       (lambda (eval-file "/scripts/event/hostile/0/0.lisp")))
   ;; Sometimes, procedurally generate an enemy. More frequently at lower levels.
   (if (< (choice 100) (get '(60 55 50 45) (zone)))
       (progn
         (push 'enemies-seen -1)
         procgen)
     (eval-file "/scripts/event/hostile_pick_template.lisp"))))


;; Just so we avoid evaluating another file while already evaluating a
;; relatively large expression. Unlikely to be problematic, but as we're running
;; on a small embedded cpu, no need to keep objects alive longer than needed.
(next)

(unbind 'next)

(adventure-log-add 3 '())

(defn on-crew-died
  (adventure-log-add 4 (list $0)))

(let ((vfn on-victory) ; save cached copy of on-victory hook in case already set
      (c (coins)))
  (defn on-victory
    (adventure-log-add 2 (list (- (coins) c) (coins-victory)))
    (when vfn
      (vfn))))


(gc)
