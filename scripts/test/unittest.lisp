;;;
;;; unittest.lisp
;;;
;;; This test case file intends to test the behavior of the lisp interpreter and
;;; standard library. For extended functions related to game logic speficially,
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
    (error "something has gone terribly wrong"))

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
    (error "something is wrong with assert-eq..."))

(if (not (error? (error "...")))
    (fatal "unable to raise errors!?"))

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

;; basic math
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

(assert-eq 1 (mod 10 3))

;; FIXME: this test case breaks syntax highlighting in emacs.
;(assert-eq 0xffffffff (| 0xff00ff00 0x00ff00ff))
(assert-eq 0x22222222 (& 0x33333333 0xaaaaaaaa))
(assert-eq 0xffff0000 (~ 0xffff))
(assert-eq 0x55555555 (^ 0xaaaaaaaa 0xffffffff))

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
;; shorter, so some extended utf8 chars will not be optimized and, instead,
;; require an extra allocation. But looking at the list of four byte untf8
;; charsets... is anyone going to care if emojis, egyptian heiroglyphics, or
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
(assert-v (error? (foo 'abcd)))           ; pass invalid type
(assert-v (error? ((compile foo) 'abcd))) ; compilation preserves type signature

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
(assert-v (error? temp)) ;; should raise undefined variable error

(assert-eq 10100 (apply + (map (curry * 2) (range 0 101))))

(defn cons (a b)
  (list a b))
(assert-eq '(5 6) (cons 5 6)) ;; you may override builtins
(unbind 'cons)
(assert-v (not (error? cons))) ;; the builtin still exits when deleting the override value
(unbind 'cons)
(assert-v (not (error? cons))) ;; you may not delete a builtin
(assert-eq '(5 . 6) (cons 5 6)) ;; the builtin function is back

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
(assert-v (nil? test-var)) ; Declared global variables are initialized to nil
(unbind 'test-var)
(assert-v (error? (setq test-var 8))) ; Write to undefined variable raises error

(global 'temp)
(setq temp (read "(lambda (a b c) (+ a b c))"))
(eval temp)
;; NOTE: eval is destructive in some cases. Normally, you don't need to worry
;; about this. But we want test coverage for these sorts of weird cases.
;; Really, when doing argument substitution, we shouldn't be modifying the input
;; list. Nothing should modify lists. But for practical purposes, it creates a
;; whole bunch of pressure on the gc if we need to clone an entire function
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
  ;; FIXME: the string is ascii, so (length str) works here... but it's not good
  ;; style.
  (assert-eq str (bytes-to-string (buffer-read temp 6 (length str)))))

(assert-eq (buffer-read temp 0 5) '(1 2 3 4 255)) ; make sure it's still there...

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
