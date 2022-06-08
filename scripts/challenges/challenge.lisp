;;;
;;; challenge.lisp
;;;

(eval-file "/scripts/reset_hooks.lisp")

(if (bound 'challenge-hint) (unbind 'challenge-hint))

(gc)

(eval-file "/scripts/challenges/index.lisp")
