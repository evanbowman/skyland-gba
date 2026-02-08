;;;
;;; async-test.lisp
;;;

(global 'put 'temp)

(setq put log)

(global 'current-test)

(defn assert-v (v)
  (when (not v)
    (error (format "In test %: assert failed! %" current-test v))))

(defn assert-eq (lhs rhs)
  (when (not (equal lhs rhs))
    (error (format "In test %: expected % not equal %"
                   current-test
                   lhs
                   rhs))))

(defn begin-test (name)
  (setq current-test name)
  (let ((msg (string "Async-test: " name "...")))
    (when (bound? 'regr-print)
      (regr-print msg 1 3))
    (put msg)))

(defn end-test ()
  (put " passed!")
  (setq current-test nil))


(global 'async-done)
(setq async-done false)

(defn bad ()
  3)

;; Cannot await a non-promise object
(assert-v (error? ((lambda () (await (bad))))))
;; Cannot await from a function called by native code
(assert-v (error? (map (lambda (_) (await (wait* 1))) '(1 2 3))))
(assert-v (error? ((compile (lambda (cb) (cb))) (lambda () (await (wait* 1))))))

(enter-stress-gc-mode)

(setq temp 9999)

;; Launch and suspend a function, to be resumed upon completion of the other
;; test cases.
(let ((val 10))
  ((lambda ()
     ;; NOTE: test-delay increments a counter for each call. It results in 27,
     ;; based on the number of times it was called in subsequent tests. Add the
     ;; input arg and assert to check that the above scope is preserved.
     (assert-eq (+ val (await (test-delay 10000))) (+ (incr temp) val))
     (setq async-done true)
     (regr-print "" 1 3) ; clear
     )))


(defn async-test ()
  (begin-test "basic...")
  (let ((x 0))
    (await (test-delay 5000))
    (strict-mode 1) ; disabled previously at bottom of script
    (+= x 1)
    (await (test-delay 2000))
    (+= x 3)
    (await (test-delay 2000))
    (assert-eq x 4))
  (end-test)

  (begin-test "param")
  ;; NOTE: test-delay returns an incremented counter for each call
  (assert-eq '(1 2 3 3) (list 1 2 3 (await (test-delay 2000))))
  (assert-eq 6 ((lambda (a b)
                  (a b))
                (lambda (c)
                  (+ c (await (test-delay 60))))
                2))
  (end-test)

  (begin-test "test loop")
  (let ((num 0))
    (while (< num 20)
      (await (test-delay 80))
      (setq num (+ num 1)))
    (assert-eq num 20))
  (end-test)

  (begin-test "nested await")
  (assert-eq 5 (length ((lambda (n)
                          (await (test-delay 90))
                          (range (/ n 2)))
                        10)))
  (end-test)

  (begin-test "compiled code")
  ;; Await works withing compiled lambdas
  (assert-eq 156 ((compile (lambda (n)
                             (* n (await (test-delay 50)))))
                  6))
  ;; Await used in a compiled lambda invoked by a different compiled lambda
  ;; should fail.
  (assert-v (error? ((compile (lambda (foo)
                                (foo)))
                     (compile (lambda ()
                                (await (wait* 12)))))))
  (end-test)

  (begin-test "lexical bindings")
  (let ((a 2) (b 5) (c 6) (d 1))
    (let ((a 1) (b 2) (c 3))
      (setq temp (await (test-delay 100)))
      (assert-eq (list a b c d) '(1 2 3 1))))
  (end-test)

  (begin-test "parallel")
  (exit-stress-gc-mode))


(async-test)

;; The interpreter detects excess values on the stack when running this script,
;; because we basically have two suspended functions that were started from the
;; top level. In normal situations, this is not a concern, because the only
;; thing that could resume execution in an idle state would be an async
;; function, which replaces the old stack restores its own stack. Long story
;; short, we need to turn off strict mode here.
(strict-mode 0)
