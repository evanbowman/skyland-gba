;;;
;;; unittest.lisp
;;;
;;; This test case file intends to test the behavior of the LISP interpreter and
;;; standard library. For extended functions related to game logic specifically,
;;; see apitest.lisp.
;;;


(gc)

(strict-mode true)

(global 'put 'temp)
(setq put log)

(global 'current-test)


(defn ensure (result)
  (when (error? result)
    (fatal result)))


(defn assert-v (v)
  (when (not v)
    (error (format "in test %: assert failed! %" current-test v))))

(if (not (error? (assert-v false)))
    (error "Something has gone terribly wrong!"))

(defn assert-eq (lhs rhs)
  (when (not (equal lhs rhs))
    (error (format "in test %: expected % not equal %"
                   current-test
                   lhs
                   rhs))))

;; At the beginning, assert the expected operand stack size. We'll have another
;; assertion at the end of regression in apitest.lisp, to make sure that we
;; aren't forgetting to pop anything off the stack.
(assert-eq 6 (lisp-mem-stack-used))

(if (not (error? (assert-eq 1 2)))
    (error "Something is wrong with assert-eq..."))

(if (not (error? (error "...")))
    (fatal "Unable to raise errors!?"))

;; First, test the functions used in the implementations of the assert
;; functions. If they don't work, then the rest is pointless.
(unless (and (not 0) (not nil))
  (error "broken not"))

(when (not 1)
  (error "broken not"))

(unless (equal '(1 2 ("abc") 3.5) '(1 2 ("abc") 3.5))
  (error "broken equal"))

(when (equal '(1 2 ("abc") 3.5) '(1 2 ("def") 3.5))
  (error "broken equal"))

(when (equal 0 1)
  (error "broken equal"))


(defn begin-test (name)
  (setq current-test name)
  (let ((msg (string "running tests: " name "...")))
    (when (bound? 'regr-print)
      (regr-print msg 1 3))
    (put msg)))

(defn end-test ()
  (put " passed!")
  (gc)
  (setq current-test nil))


(assert-v (error? (let ((foo 2)) (global 'foo))))
(assert-v (error? ((lambda () (setq a 5) 8))))



(begin-test "reader")

(assert-eq 1 (read "1"))
(assert-eq (cons 1 2) (read "(1 . 2)"))

(end-test)


(begin-test "math")

;; Basic math
(assert-eq 5.0 (* 2.5 2.0))
(assert-eq 10 (* 2 5))
(assert-eq 1 (*))
(assert-eq 0 (+))
(assert-eq 5050 (apply + (range 0 101)))
(assert-eq -5 (- 5))
(assert-eq 2 (/ 10 5))
(assert-eq -5.0 (- 5.0))
(assert-eq .5 (* 0.25 2.0))

;; Test for errors on incompatible float/int math
(assert-v (error? (- 5 5.0)))
(assert-v (error? (- 5.0 5)))
(assert-v (error? (+ 5 5.0)))
(assert-v (error? (+ 5.0 5)))
(assert-v (error? (* 5.0 5)))
(assert-v (error? (* 5 5.0)))
(assert-v (error? (/ 5.0 5)))
(assert-v (error? (/ 5 5.0)))

(assert-v (float? 0.05))
(assert-v (float? -.6))
(assert-v (int? 22))
(assert-v (int? -22))
(assert-v (int? -0))

(assert-v (boolean? nil))
(assert-v (boolean? true))
(assert-v (boolean? false))
(assert-v (not (boolean? 55)))

(assert-eq 1 (mod 10 3))

(assert-eq 0xffffffff (bit-or 0xff00ff00 0x00ff00ff))
(assert-eq 0x22222222 (bit-and 0x33333333 0xaaaaaaaa))
(assert-eq 0xffff0000 (bit-not 0xffff))
(assert-eq 0x55555555 (bit-xor 0xaaaaaaaa 0xffffffff))

(assert-eq 0xaff (read (hex 0xaff)))

(end-test)



(begin-test "string")

(assert-eq (length "ありがとうございます") 10)
(assert-eq (slice "ありがとうございます" 2 7) "がとうござ")
(assert-eq (string 5 12 "foo" 3.5 'nice) "512foo3.5nice")
(assert-eq "你好" (string-assemble (string-explode "你好")))

(setq temp (lisp-mem-string-storage))
"abc123"
;; After allocating the above string, internal string memory should have
;; increased by seven bytes, i.e. the length of the string plus one null byte.
(assert-eq (lisp-mem-string-storage) (+ temp 7))
"y"
;; The interpreter does an internal buffer optimization for small strings, which
;; is the reason why there is no character datatype. Although... technically the
;; internal buffer optimization only covers strings that are three bytes or
;; shorter, so some extended UTF-8 chars will not be optimized and, instead,
;; require an extra allocation. But looking at the list of four byte UTF-8
;; charsets... is anyone going to care if emojis, egyptian hieroglyphs, or
;; musical symbols are not hyper-optimized?
(assert-eq (lisp-mem-string-storage) (+ temp 7))
"yy"
(assert-eq (lisp-mem-string-storage) (+ temp 7))
"yyy"
(assert-eq (lisp-mem-string-storage) (+ temp 7))
"yyyy" ;; This should trigger an allocation of five additional bytes...
(assert-eq (lisp-mem-string-storage) (+ temp 12))

(end-test)



(begin-test "list")

(assert-eq 3 (length (cons 1 (cons 2 (cons 3 nil)))))
(assert-eq -2 (apply - (reverse '(3 1))))
(assert-eq 8640 (apply * (flatten '((1 1 2) 3 (5 (1 1 (2 3) 6) 8)))))
(assert-eq '(10 11 12 13 14 0 1 2 3 4) (difference (range 0 10) (range 5 15)))
(assert-eq '(2 3) (union (range 10) '(-1 2 2 3 12 12 14)))
(assert-v ((pos-equalto? 1 5) '(1 5 7)))
(assert-eq (car '(2 . 3)) (first '(2 . 3)))
(assert-eq (cdr '(4 . 5)) (second '(4 . 5)))

(end-test)



(begin-test "closure")

(assert-eq 10 (((lambda (a b c)
                  (lambda (d e) (+ a d e)))
                1 2 3)
               4 5))

(setq temp 1)

(let ((temp 4))
  (defn foo ()
    (setq temp (+ temp 1))
    temp))

(assert-eq 5 (foo))
(assert-eq 6 (foo))
(assert-eq 7 (foo))
(assert-eq 8 (foo))
(assert-eq 9 (foo))
(assert-eq 10 (foo))
(assert-eq temp 1)

(assert-eq 135 (apply + (map foo (range 9))))

(unbind 'foo)

(end-test)


(begin-test "type signature")

(defn foo ((arg . int))
  (* arg 2))

(assert-eq (foo 2) 4)
(assert-v (error? (foo 'abcd)))           ;; Pass invalid type.
(assert-v (error? ((compile foo) 'abcd))) ;; Compilation preserves type signature.

(defn foo ((str . string) (iv . int))
  (format str iv iv))

(assert-eq (foo "%:%" 5) "5:5")
(assert-v (error? (foo 5 6)))
(assert-v (error? (foo "5" "6")))

(assert-eq (signature foo) '(? string int ? ?))

(end-test)



(begin-test "misc")

(assert-eq (range 0 20) (sort (reverse (range 0 20)) <))
(assert-eq (reverse (range 0 20)) (sort (range 0 20) >))
(setq temp 99)
(assert-eq temp 99)
(unbind 'temp)
(assert-v (error? temp)) ;; Should raise undefined variable error.

(assert-eq 10100 (apply + (map (curry * 2) (range 0 101))))

(defn cons (a b)
  (list a b))
(assert-eq '(5 6) (cons 5 6)) ;; You may override built-ins.
(unbind 'cons)
(assert-v (not (error? cons))) ;; The built-in still exits when deleting the override value.
(unbind 'cons)
(assert-v (not (error? cons))) ;; You may not delete a built-in.
(assert-eq '(5 . 6) (cons 5 6)) ;; The built-in function is back.

(end-test)


(begin-test "library")

(assert-eq '(b . 6) (assoc 'b (acons 'b 6 '((b . 7) (c . 8)))))
(assert-eq 190 (apply + (append (range 10) (range 10 20))))
(assert-eq '(5 6 7 8) (slice '(1 2 3 4 5 6 7 8) 4 8))
(assert-eq '(2 3 4) (slice '(1 2 3 4) 1))
(assert-eq 8 (caar '((8))))
(assert-eq 2 (cadr (list 1 2)))
(assert-v (filter (equalto? 'sauce) '(1 4.7 sauce "mellow")))
(assert-eq 35 ((curry + 5 6) 7 8 9))
(assert-eq 45 (apply + (fill 45 1)))
(assert-eq 12397 (int (apply string (filter string? '(1 2 "123" 4 5 "97")))))
(assert-eq 3 (get '(1 2 3 4 5) 2))
(assert-eq 5 (length (range 5)))
(assert-eq 0 (min (reverse (range 10))))
(assert-eq 9 (max (range 10)))
(assert-eq '(1 0 2 0 3 0) (replace '(1 a 2 b 3 c) symbol? 0))

(assert-eq 5 (abs -5))
(assert-eq "a 12 3.5 '(1 2 3) " (format "% % % % " 'a 12 3.5 '(1 2 3)))
(assert-eq 20 (apply + (map int (split "0,1,1,2,3,5,8" ","))))
(assert-eq (map cons '(1 2 3) '(4 5 6)) '((1 . 4) (2 . 5) (3 . 6)))
(assert-eq 'cake (symbol "cake"))
(assert-v (not (equal 'cake 'CAKE)))
(assert-v ((equalto? 9) 9))
(assert-v "bats" ((lambda () (arg 1)) "birds" "bats" "iguana"))
(assert-eq cons (identity cons))
(assert-v (error? ((require-args (lambda () nil) 2) 0)))
(defn test-this ()
  (this))
(assert-eq test-this (test-this))

(assert-v ((notequal? 5) 7))
(assert-v (not ((notequal? 6) 6)))

(assert-v ((car-equalto? 11) '(11 10 9 8)))
(assert-v ((pos-equalto? 1 15) '(3 15 7)))
(assert-v (not ((pos-equalto? 1 8) '(3 15 7))))



(let ((tmp 0))
  (defn foo ()
    (if (< tmp 10)
        (progn
          (+= tmp 1)
          tmp)
      nil)))

(assert-eq 55 (apply + (collect foo)))

(end-test)


(begin-test "core")
;; These test cases are testing very obscure implementation details about the
;; interpreter, which normally wouldn't be visible to the programmer unless
;; you're really doing something strange.

(global 'test-var)
(assert-v (nil? test-var)) ;; Declared global variables are initialized to nil.
(unbind 'test-var)
(assert-v (error? (setq test-var 8))) ;; Write to undefined variable raises error.

(global 'temp)
(setq temp (read "(lambda (a b c) (+ a b c))"))
(eval temp)
;; NOTE: eval is destructive in some cases. Normally, you don't need to worry
;; about this. But we want test coverage for these sorts of weird cases.
;; Really, when doing argument substitution, we shouldn't be modifying the input
;; list. Nothing should modify lists. But for practical purposes, it creates a
;; whole bunch of pressure on the GC if we need to clone an entire function
;; implementation every time we do argument substitution.
(assert-eq temp '(lambda (a b c) (+ $0 $1 $2)))

;; Another thing that you don't typically see: lambda is not the lowest level
;; function primitive. There's a lower level syntax element called 'fn, which
;; all lambdas are converted to after argument substitution. fn has no argument
;; list and all substituted arguments have been replaced by $<argument number>.
(assert-eq (disassemble (eval temp)) '(fn (+ $0 $1 $2)))

;; Now a more complex one with a nested lambda:
(setq temp (read "(lambda (a b c) (let ((x a) (y (+ b c))) (lambda (w v) (+ w v x y))))"))
(assert-eq (disassemble (eval temp)) '(fn (let ((x $0) (y (+ $1 $2))) (fn (+ $0 $1 x y)))))

;; Ok, now this gets really nasty. Because we're substituting function arguments
;; for stack slots, returning a lambda that forms a closure over arguments
;; doesn't work, because the stack no longer exists. So we use a bad hack where
;; we generate a synthetic let binding representing the function arguments that
;; need to be injected into the closure.
(assert-eq (disassemble (lambda (a b c)
                          (lambda (d e) (+ a b d e))))
           '(fn (let ((a $0)
                      (b $1))
                  (fn (+ a b $0 $1)))))

;; Let's make sure that argument closure nesting works correctly...
(assert-eq (disassemble (lambda (a)
                          (lambda (b)
                            (lambda (c)
                              (+ a b c)))))
           '(fn (let ((a $0))
                  (fn (let ((b $0))
                        (fn (+ a b $0)))))))
;; The above stuff is such a mess... but the important thing is that nobody ever
;; really needs to know how it actually works when they're writing scripts,
;; unless for some reason they're disassembling functions and messing around
;; with the reader...


;; Test some macros... the reader eagerly expands macros. This is pretty bad,
;; but again, macroexpanding stuff during evaluation every time isn't
;; sensible...
(assert-eq (read "(and 1 2 3)") '(if (not 1) 0 (if (not 2) 0 (if (not 3) 0 1))))
(assert-eq (read "(or 1 2)") '(if 1 1 (if 2 1 0)))
(assert-eq (read "(when true nil)") '(if 1 (let () ())))
(assert-eq (read "(cond ((a 5) 6 7 8) (true nil))")
           '(if (a 5) (let () 6 7 8) (if 1 (let () ()) ())))

;; The $V symbol gives you access to a variadic argument list. Let's make sure
;; it works...
(assert-eq 31 ((lambda (a b) (apply + (append (list a b) $V))) 1 2 3 4 5 6 7))
(assert-eq 4 ((lambda () (length $V)) 1 2 3 4))

(assert-eq true 1)
(assert-eq nil false)
(assert-eq nil '())


(assert-v (not (error? (eval (read "(defn temp (a b c d e) (+ a b c d e))")))))

(let ((result (eval (read "(defn temp (a b c d e f) (+ a b c d e f))"))))
  (assert-v (and (error? result)
                 (equal (error-info result)
                        "no more than 5 named args allowed in function"))))

(assert-eq
 9
 (case (+ 5 1)
   (5 0)
   (1 0)
   (6 9)))

(assert-eq (read "(case (+ 5 1) (5 0) (6 9) (else 55))")
           '(let ((--TEMP-CASE-V (+ 5 1)))
             (if (equal --TEMP-CASE-V 5)
                 (let () 0)
                 (if (equal --TEMP-CASE-V 6)
                     (let () 9)
                     (let () 55)))))

(assert-eq
  3
  (let ((x 0))
    (case 5
      (5 (setq x 1) (setq x 2) (setq x 3) x)
      (else 0))))


(assert-eq nil
  (case 5
    (1 "a")
    (2 "b")))

(assert-eq "first"
  (case 5
    (5 "first")
    (5 "second")
    (else "third")))

(let ((counter 0))
  (defn incr-and-return ()
    (+= counter 1)
    counter)

  (case (incr-and-return)
    (1 "one")
    (2 "two")
    (else "other"))

  (assert-eq 1 counter))  ;; Should be 1, not 2 or more


(end-test)



(begin-test "databuffer")

(setq temp (lisp-mem-sbr-used))
(databuffer)
;; Make sure that allocating a databuffer increases the scratch buffer (sbr) count
(assert-eq (lisp-mem-sbr-used) (+ temp 1))
(gc)
;; The databuffer that we created above has no references to it. Make sure that
;; we recovered the mem associated with it.
(assert-eq (lisp-mem-sbr-used) temp)

(setq temp (databuffer))
(assert-v (buffer-write! temp 0 '(1 2 3 4 255)))
(assert-eq (buffer-read temp 0 5) '(1 2 3 4 255))

(let ((str "Hello, there!"))
  (buffer-write! temp 6 (string-to-bytes str))
  ;; FIXME: The string is ASCII, so (length str) works here... but it's not good
  ;; style.
  (assert-eq str (bytes-to-string (buffer-read temp 6 (length str)))))

(assert-eq (buffer-read temp 0 5) '(1 2 3 4 255)) ;; Make sure it's still there...

(assert-eq 2147483647 (bytes-to-int (int-to-bytes 2147483647)))

(let ((str "こんにちは！"))
  (let ((encoded (string-to-bytes str)))
    (buffer-write! temp 0 encoded)
    (assert-eq str (bytes-to-string (buffer-read temp 0 (length encoded))))))


(end-test)



(begin-test "wrapped")

;; Here, we're going to define a custom value type called widget. The wrap
;; function allows us to define custom types.

(defn -decorate-widget (w)
  (format "#(widget:%)" (unwrap w)))

(setq temp (wrap 30 'widget))
(assert-eq 'widget (type temp))
(assert-eq 30 (unwrap temp))
(assert-eq "#(widget:30)" (string temp))

(defn -equal-widget (w1 w2)
  (equal (unwrap w1) (unwrap w2)))

(assert-v (equal temp (wrap 30 'widget)))
(assert-v (not (equal temp (wrap 50 'widget))))

(unbind '-equal-widget
        '-decorate-widget)

(end-test)


(begin-test "syntax")

(assert-eq "invalid let binding list" (error-info (lint '(let (a 5) a))))
(assert-eq "malformed let expr!" (error-info (lint '(let ))))
(assert-eq "let binding missing symbol" (error-info (lint '(let ((a 6) (7 8))))))
(assert-eq "invalid value in lambda arglist!" (error-info (lint '(lambda (1)))))
(assert-eq "invalid lambda syntax!" (error-info (lint '(lambda ))))
(assert-eq "invalid while syntax!" (error-info (lint '(while ))))

(end-test)


(begin-test "constants")

(defconstant test-const 5)
(assert-eq test-const 5)

;; NOTE: you may initialize a constant with a runtime expression, but if you do,
;; that runtime expression will be evaluated every time that the constant is
;; accessed. Arguably, this behavior makes constants not really constant, but
;; it's needed to allow constant definition in terms of other constants.
(defconstant test-const-2 (string (* test-const 2)))
(assert-eq test-const-2 "10")

(end-test)



(begin-test "edge-cases")

;; Empty list operations
(assert-eq 0 (length '()))
(assert-eq '() (reverse '()))
(assert-eq '() (filter (lambda (x) true) '()))
(assert-eq '() (map (lambda (x) (* x 2)) '()))

;; Single element operations
(assert-eq 1 (length '(5)))
(assert-eq '(5) (reverse '(5)))
(assert-eq 5 (min '(5)))
(assert-eq 5 (max '(5)))

;; Nested empty structures
(assert-eq '() (flatten '(() () ())))
(assert-eq '(1 2) (flatten '(() 1 () 2 ())))

;; String edge cases
(assert-eq 0 (length ""))
(assert-eq "" (slice "" 0 0))
(assert-eq '() (split "" ","))

;; Range edge cases
(assert-eq '() (range 5 5))
(assert-eq '() (range 10 0))

(end-test)


(begin-test "recursion")

;; Fibonacci
(defn fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(assert-eq 55 (fib 10))

;; List length (recursive)
(defn len-rec (lst)
  (if (nil? lst)
      0
      (+ 1 (len-rec (cdr lst)))))

(assert-eq 5 (len-rec '(1 2 3 4 5)))
(assert-eq 0 (len-rec '()))

(unbind 'factorial 'fib 'len-rec)

(end-test)


(begin-test "list-manipulation")

;; Insert at various positions
(assert-eq '(99 1 2 3) (insert 99 '(1 2 3) 0))
(assert-eq '(1 99 2 3) (insert 99 '(1 2 3) 1))
(assert-eq '(1 2 3 99) (insert 99 '(1 2 3) 3))

;; Find various elements
(assert-eq 0 (find 1 '(1 2 3)))
(assert-eq 2 (find 3 '(1 2 3)))
(assert-eq nil (find 99 '(1 2 3)))

;; Association lists
(setq temp '((a . 1) (b . 2) (c . 3)))
(assert-eq '(b . 2) (assoc 'b temp))
(assert-eq nil (assoc 'd temp))
(assert-eq '((d . 4) (a . 1) (b . 2) (c . 3)) (acons 'd 4 temp))

;; Replace with predicate
(assert-eq '(0 2 0 4 0) (replace '(1 2 3 4 5) odd? 0))

(end-test)


(begin-test "special-variables")

;; arg function
(defn arg-test ()
  (list (arg 0) (arg 1) (arg 2)))

(assert-eq '(1 2 3) (arg-test 1 2 3))

(unbind 'varargs-test 'arg-test)

(end-test)



(begin-test "sorting-comparisons")

;; Sort with custom comparator
(defn compare-second (a b)
  (< (cdr a) (cdr b)))

(assert-eq '((c . 1) (a . 2) (b . 3))
           (sort '((a . 2) (b . 3) (c . 1)) compare-second))

;; Sort strings (if supported)
;; (assert-eq '("a" "b" "c") (sort '("c" "a" "b") string<?))

;; Already sorted list
(assert-eq (range 0 10) (sort (range 0 10) <))

;; Reverse sorted
(assert-eq (reverse (range 0 10)) (sort (range 0 10) >))

(unbind 'compare-second)

(end-test)


(begin-test "memory-stress")

;; Large list operations
(let ((temp (range 100)))
  (assert-eq 100 (length temp))
  (assert-eq 4950 (apply + temp)))


;; String manipulation
(setq temp (apply string (map (lambda (x) (format "%" x)) (range 0 10))))
(assert-eq "0123456789" temp)

(end-test)


(begin-test "let-bindings")

;; Nested lets
(assert-eq 6
  (let ((x 1))
    (let ((y 2))
      (let ((z 3))
        (+ x y z)))))

;; Shadowing
(setq temp 99)
(assert-eq 5
  (let ((temp 5))
    temp))
(assert-eq 99 temp)

;; Multiple bindings
(assert-eq 10
  (let ((a 1) (b 2) (c 3) (d 4))
    (+ a b c d)))

;; Let with no bindings
(assert-eq 5
  (let ()
    5))

(end-test)


(begin-test "boolean-logic")

;; And short-circuit
(setq temp 0)
(defn side-effect ()
  (setq temp 1)
  true)

(and false (side-effect))
(assert-eq 0 temp) ;; Should not have called side-effect

(and true (side-effect))
(assert-eq 1 temp)

;; Or short-circuit
(setq temp 0)
(or true (side-effect))
(assert-eq 0 temp) ;; temp still 1 from before, not called again

(setq temp 0)
(or false (side-effect))
(assert-eq 1 temp) ;; Now it should be called

;; Nested boolean logic
(assert-v (and true (or false true)))
(assert-v (not (and true (or false false))))
(assert-v (or (and false true) (and true true)))

(unbind 'side-effect 'temp)

(end-test)


(begin-test "numeric-edge-cases")

;; Zero operations
(assert-eq 0 (* 5 0))
(assert-eq 0 (* 0 5))
(assert-eq 0 (+ 0 0))
(assert-eq 0 (- 0 0))

;; Negative numbers
(assert-eq -5 (- 0 5))
(assert-eq -10 (* -2 5))
(assert-eq -10 (* 2 -5))
(assert-eq 10 (* -2 -5))
(assert-eq -3 (+ -5 2))

;; Modulo with negatives (behavior may vary)
;; (assert-eq ? (mod -10 3))
;; (assert-eq ? (mod 10 -3))

;; Float precision
(assert-v (< (abs (- (* 0.1 10.0) 1.0)) 0.01))

(end-test)


(begin-test "symbol-manipulation")

;; Symbol creation and comparison
(assert-eq 'test (symbol "test"))
(assert-v (not (equal 'test 'TEST)))
(assert-v (symbol? 'test))
(assert-v (symbol? (symbol "dynamic")))

(end-test)



(assert-v (bound? 'begin-test))
(assert-v (bound? 'end-test))

(unbind 'begin-test
        'temp
        'ensure
        'end-test)

(assert-v (not (bound? 'begin-test)))
(assert-v (not (bound? 'end-test)))

(unbind 'assert-v
        'assert-eq)

(if (lambda? log) (unbind 'put))

(gc)
