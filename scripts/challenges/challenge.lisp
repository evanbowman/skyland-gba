;;;
;;; challenges/challenge.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")

(if (bound? 'challenge-hint) (unbind 'challenge-hint))

(engine-set "powerdown_allowed" 0)
(engine-set "rewind_disabled" 1)

(eval-file "/scripts/challenges/index.lisp")
