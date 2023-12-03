
(when (not (bound? 'put))
  (setq put log)
  (setq newline (lambda nil)))


(defn assert-v [1]
  (when (not $0)
    (newline)
    (error (format "assert failed! %" $0))))

(defn assert-eq [2]
  (when (not (equal $0 $1))
    (newline)
    (error (format "failure! expected % not equal %"
                   $0
                   $1))))

(if (not (error? (error "...")))
    (fatal "unable to raise errors!?"))

;; First, test the functions used in the implementations of the assert
;; functions. If they don't work, then the rest is pointless.
(unless (not 0)
  (error "broken not"))

(when (not 1)
  (error "broken not"))

(unless (equal '(1 2 ("abc") 3.5) '(1 2 ("abc") 3.5))
  (error "broken equal"))

(when (equal 0 1)
  (error "broken equal"))


(defn begin-test [1]
  (put (string $0 " test cases...")))

(defn end-test [0]
  (put " passed!")
  (newline))


(begin-test "READER")

(assert-eq 1 (read "1"))
(assert-eq (cons 1 2) (read "(1 . 2)"))

(end-test)



(begin-test "MATH")

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

(end-test)



(begin-test "STRING")

(assert-eq (length "ありがとうございます") 10)
(assert-eq (substr "ありがとうございます" 2 7) "がとうござ")
(assert-eq (string 5 12 "foo" 3.5 'nice) "512foo3.5nice")

(end-test)



(begin-test "LIST")

(assert-eq 3 (length (cons 1 (cons 2 (cons 3 nil)))))
(assert-eq -2 (apply - (reverse '(3 1))))

(end-test)



(begin-test "CLOSURE")

(setq temp 1)

(let ((temp 4))
  (defn foo [0]
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

(end-test)


(begin-test "LAMBDA")

(assert-eq 5 (length ((lambda $V) 2 2 2 2 2)))

(end-test)



(begin-test "MISC")

(assert-eq (range 0 10) (sort (reverse (range 0 10)) <))
(setq temp 99)
(assert-eq temp 99)
(unbind 'temp)
(assert-v (error? temp)) ;; should raise undefined variable error

(assert-eq 10100 (apply + (map (curry * 2) (range 0 101))))

(defn cons [2]
  (list $0 $1))
(assert-eq '(5 6) (cons 5 6)) ;; you may override builtins
(unbind 'cons)
(assert-v (not (error? cons))) ;; the builtin still exits when deleting the override value
(unbind 'cons)
(assert-v (not (error? cons))) ;; you may not delete a builtin
(assert-eq '(5 . 6) (cons 5 6)) ;; the builtin function is back

(end-test)


(begin-test "LIBRARY FUNCTIONS")

(assert-eq '(b . 6) (assoc 'b (acons 'b 6 '((b . 7) (c . 8)))))
(assert-eq 190 (apply + (append (range 10) (range 10 20))))
(assert-eq '(5 6 7 8) (cdr (bisect '(1 2 3 4 5 6 7 8))))
(assert-eq 8 (caar '((8))))
(assert-eq 2 (cadr (list 1 2)))
(assert-v (contains '(1 4.7 sauce "mellow") 'sauce))
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
(assert-eq 'cake (symbol "cake"))
(assert-v ((equalto? 9) 9))
(assert-v "bats" ((lambda (arg 1)) "birds" "bats" "iguana"))
(assert-eq cons (identity cons))
(assert-v (error? ((require-args (lambda nil) 2) 0)))
(defn test-this [0]
  (this))
(assert-eq test-this (test-this))

(end-test)



(unbind 'assert-v
        'assert-eq
        'begin-test
        'end-test)

(if (lambda? log) (unbind 'put))
