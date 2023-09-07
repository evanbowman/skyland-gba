;;;
;;; stdlib.lisp
;;;
;;; A small standard library
;;;


(macro or (EXPR)
 `(if ,(car EXPR)
      1
    ,(if (cdr EXPR)
         (cons 'or (cdr EXPR))
       0)))


(macro and (EXPR)
 `(if (not ,(car EXPR))
      0
    ,(if (cdr EXPR)
         (cons 'and (cdr EXPR))
       1)))


(macro cond (EXPR)
 `(if ,(car (car EXPR))
      ,(cons 'progn (cdr (car EXPR)))
    ,(if (cdr EXPR)
         (cons 'cond (cdr EXPR))
       nil)))


(macro repeat (N BODY)
 `(map (lambda ,@BODY) (range 0 ,N)))


;; Some useful macros for defining functions

;; Defines a function.
(macro defn (NAME BODY) `(setq ,NAME (lambda ,@BODY)))
;; Defines a bytecode-compiled function.
(macro defn/c (NAME BODY) `(setq ,NAME (compile (lambda ,@BODY))))

(macro += (NAME VAL)
 `(setq ,NAME (+ ,NAME ,@VAL)))

(macro setq (NAME EXPR)
 `(set ,(cons $q NAME) ,@EXPR))

(macro when (EXPR BODY) `(if ,EXPR (progn ,@BODY)))
(macro unless (EXPR BODY) `(if (not ,EXPR) (progn ,@BODY)))


;; A shortcut for defining functions with named arguments. e.g.:
;; (defun foo (a b c)
;;   (+ a b c))
;; Currently disabled, as named arguments require more memory and I personally
;; don't mind refering to arguments by number.
;; (macro defun (NAME ARGS BODY) `(set ,(cons $q NAME) (fn ,ARGS ,@BODY)))


;; Because we're running lisp in an embedded system (a gameboy) with limited
;; memory, we need to be really careful about symbol table usage, which is why,
;; traditionally, we only support numbered arguments for lambdas. But this
;; function macro allows you to declare functions with named arguments:
(macro fn (ARGS BODY)
 (if (not ARGS)
     `(lambda ,@BODY)
   `(lambda
     (let ,((lambda
             (if (not $0)
                 $1
               ((this)
                (cdr $0)
                (cons (list (car $0) (symbol (string "$" $2))) $1)
                (+ $2 1))))
            ARGS nil 0)
       ,@BODY))))


(macro progn (BODY)
 `(let () ,@BODY))


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
  (cond
   ((not $0) $1)
   ((not $1) $0)
   (($2 (car $0) (car $1))
    (cons (car $0) ((this) (cdr $0) $1 $2)))
   (true (cons (car $1) ((this) $0 (cdr $1) $2)))))


(defn/c sort
  (if (not (cdr $0))
      $0
    (let ((temp (bisect $0)))
      (merge (sort (car temp) $1)
             (sort (cdr temp) $1)
             $1))))


;; While suboptimal, these functions have the benefit of being small.
(defn/c min (car (sort $0 <)))
(defn/c max (car (sort $0 >)))

(defn/c replace
  ;; (lat value new-value)
  (let ((v $1)
        (newv $2))
    (map
     (lambda
       (if (equal $0 v)
           newv
         $0))
     $0)))
