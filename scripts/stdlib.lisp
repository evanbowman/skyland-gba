;;;
;;; stdlib.lisp
;;;
;;; A small standard library
;;;


(macro or (expr)
 `(if ,(car expr)
      1
    ,(if (cdr expr)
         (cons 'or (cdr expr))
       0)))


(macro and (expr)
 `(if (not ,(car expr))
      0
    ,(if (cdr expr)
         (cons 'and (cdr expr))
       1)))


(macro cond (expr)
 `(if ,(car (car expr))
      ,(cons 'progn (cdr (car expr)))
    ,(if (cdr expr)
         (cons 'cond (cdr expr))
       nil)))


;; Some useful macros for defining functions

;; Defines a function.
(macro defn (name body) `(setq ,name (lambda ,@body)))
;; Defines a bytecode-compiled function.
(macro defn/c (name body) `(setq ,name (compile (lambda ,@body))))


(macro setq (name expr)
 `(set ,(cons $q name) ,@expr))


;; For our onscreen keyboard, which has no + key
(setq add +)


;; Because we're running lisp in an embedded system (a gameboy) with limited
;; memory, we need to be really careful about symbol table usage, which is why,
;; traditionally, we only support numbered arguments for lambdas. But this
;; function macro allows you to declare functions with named arguments:
(macro fn (args body)
 (if (not args)
     `(lambda ,@body)
   `(lambda
     (let ,((lambda
             (if (not $0)
                 $1
               ((this)
                (cdr $0)
                (cons (list (car $0) (symbol (string "$" $2))) $1)
                (+ $2 1))))
            args nil 0)
       ,@body))))


(macro while (expr body)
 ;; The vm only performs TCO on compiled lambdas, so we need to enforce
 ;; compilation here, unfortunately. Quite slow, but then, how often do you
 ;; really _need_ a while loop in lisp?
 `((compile
    (lambda
      (if ,expr
          (let ()
            ,@body
            ((this))))))))


(macro progn (body)
 `(let () ,@body))


(defn/c acons
  (cons (cons $0 $1) $2))


(defn/c assoc
  (let ((temp $0))
    (get (filter (lambda (equal (car $0) temp))
                 $1)
         0)))


(defn append
  ;; Not the most efficient way to implement append, but this implementation
  ;; with unquote-splicing is quite compact.
  `(,@$0 ,@$1))


(defn/c push
  (set $0 (cons $1 (eval $0))))


(setq bisect
     (let ((impl (compile
                  (lambda
                    (if (not $1)
                        (cons (reverse $2) $0)
                      (if (not (cdr $1))
                          (cons (reverse $2) $0)
                        ((this)
                         (cdr $0)
                         (cdr (cdr $1))
                         (cons (car $0) $2))))))))
       (lambda (impl $0 $0 '()))))


(defn/c merge
  (if (not $0)
      $1
    (if (not $1)
        $0
      (if (< (car $0) (car $1))
          (cons (car $0) ((this) (cdr $0) $1))
        (cons (car $1) ((this) $0 (cdr $1)))))))


(defn/c sort
  (if (not (cdr $0))
      $0
    (let ((temp (bisect $0)))
      (merge (sort (car temp))
             (sort (cdr temp))))))


;; While suboptimal, these functions have the benefit of being small.
(defn/c min (car (sort $0)))
(defn/c max (car (reverse (sort $0))))
