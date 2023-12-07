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


(macro dotimes (N BODY)
 `(map (lambda ,@BODY) (range 0 ,N)))


;; Some useful macros for defining functions

;; Defines a function.
(macro defn (NAME REST)
       `(safe-setfn ,(cons $q NAME)
                    (lambda ,@(cdr REST))
                    ,(cons $q (car REST))))

;; Defines a bytecode-compiled function.  You should only compile long-lived
;; functions, because bytecode cannot be deallocated. At the same time, bytecode
;; takes up less space than non-compiled functions.
(macro defn/c (NAME REST)
       `(safe-setfn ,(cons $q NAME)
                    (compile (lambda ,@(cdr REST)))
                    ,(cons $q (car REST))))


(macro += (NAME VAL)
 `(setq ,NAME (+ ,NAME ,@VAL)))

(macro setq (NAME EXPR) `(set ,(cons $q NAME) ,@EXPR))

(macro when (EXPR BODY) `(if ,EXPR (progn ,@BODY)))
(macro unless (EXPR BODY) `(if (not ,EXPR) (progn ,@BODY)))

;; NOTE: for historical reasons, lambdas do not include syntax for specifying an
;; argument count, as this scripting language only supports numbered positional
;; arguments. Require-args was a safety feature added retrospectively, and I've
;; hacked it into the function defintion macros.
(setq safe-setfn
      (require-args
       (compile
        (lambda
          ;; safe-setfn is responsible for validating the format of data passed
          ;; to defn, and setting the function in the environment.
          ;;
          ;; Make sure that the user remembered to specify an argument count
          ;; when using one of the defn macros:
          (when (or (not (pair? $2))
                    (not (int? (car $2)))
                    (cdr $2)) ;; b/c arg count must be a list with one element
            (fatal (string $0 ": invalid defn, missing argc")))
          (set $0 (require-args $1 (car $2)))))
       3))


(macro progn (BODY)
 `(let () ,@BODY))


(defn/c acons [3]
  (cons (cons $0 $1) $2))


(defn/c assoc [2]
  (let ((temp $0))
    (get (filter (lambda (equal (car $0) temp))
                 $1)
         0)))

(defn/c cdr-assoc [2]
  (let ((kvp (assoc $0 $1)))
    (if kvp (cdr kvp))))


(defn append [2]
  ;; Not the most efficient way to implement append, but this implementation
  ;; with unquote-splicing is quite compact.
  `(,@$0 ,@$1))

(defn/c gen [2]
  (map $0 (range $1)))

(defn/c push [2]
  (set $0 (cons $1 (eval $0))))


(defn/c push-set [2]
  (let ((tmp (cons $1 (eval $0))))
    (set $0 (union tmp tmp))))


(defn/c merge [3]
  (cond
   ((not $0) $1)
   ((not $1) $0)
   (($2 (car $0) (car $1))
    (cons (car $0) ((this) (cdr $0) $1 $2)))
   (true (cons (car $1) ((this) $0 (cdr $1) $2)))))


(defn/c sort [2]
  (if (not (cdr $0))
      $0
    (let ((len (length $0)))
      (merge ((this) (slice $0 0 (/ len 2)) $1)
             ((this) (slice $0 (/ len 2)) $1)
             $1))))


;; While suboptimal, these functions have the benefit of being small.
(defn/c min [1] (car (sort $0 <)))
(defn/c max [1] (car (sort $0 >)))

(defn/c replace [3]
  ;; (lat predicate new-value)
  (let ((pred $1)
        (newv $2))
    (map
     (lambda
       (if (pred $0)
           newv
         $0))
     $0)))

(defn/c curry [1]
  (let ((func $0)
        (args (cdr $V)))
    (lambda
      (apply func (append args $V)))))

;; Return a predicate that returns true if its argument equals the supplied value.
;; e.g.: ((equalto? 2) 2) -> true
(defn/c equalto? [1]
  (curry equal $0))

(defn/c notequal? [1]
  (let ((v $0))
    (lambda
      (not (equal $0 v)))))
