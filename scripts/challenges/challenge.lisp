;;;
;;; challenge.lisp
;;;

(eval-file "/scripts/reset_hooks.lisp")

(if (bound? 'challenge-hint) (unbind 'challenge-hint))

(setvar "powerdown_allowed" 0)

(gc)

(eval-file "/scripts/challenges/index.lisp")
