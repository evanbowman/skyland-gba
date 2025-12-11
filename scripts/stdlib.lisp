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


(macro -case-r (EXPR)
 `(if (equal --TEMP-CASE-V ,(car (car EXPR)))
      ,(cons 'progn (cdr (car EXPR)))
    ,(if (cdr EXPR)
         (if (equal (car (car (cdr EXPR))) 'else)
             (cons 'progn (cdr (car (cdr EXPR))))
           (cons '-case-r (cdr EXPR)))
       nil)))

(macro case (EXPR)
       `(let ((--TEMP-CASE-V ,(car EXPR)))
          ,(cons '-case-r (cdr EXPR))))


(macro dotimes (N BODY)
 `(map (fn ,@BODY) (range 0 ,N)))


;; Some useful macros for defining functions

;; The new defn macros, to replace the old ones using positional args.
(macro defn (NAME REST) `(setfn ,(cons $q NAME) (lambda ,@REST)))

;; Defines a bytecode-compiled function.  You should only compile long-lived
;; functions, because bytecode cannot be deallocated. At the same time, bytecode
;; takes up less space than non-compiled functions.
(macro defn/c (NAME REST) `(setfn ,(cons $q NAME) (compile (lambda ,@REST))))

;; Define a temporary function, to be cleaned up at the end of a level. Use for
;; script-local functions that don't need to be referenced by other scripts.
(macro defn/temp (NAME REST) `(set-temp ,(cons $q NAME) (lambda ,@REST)))


(macro += (NAME VAL)
 `(setq ,NAME (+ ,NAME ,@VAL)))

(macro setq (NAME EXPR) `(set ,(cons $q NAME) ,@EXPR))

(macro when (EXPR BODY) `(if ,EXPR (progn ,@BODY)))
(macro unless (EXPR BODY) `(if (not ,EXPR) (progn ,@BODY)))

(global 'setfn)
(setq setfn
      (compile
       (lambda ((sym . symbol) fn)
         (global sym)
         (set sym fn))))


(global 'temp-vals)

(defn/c set-temp ((sym . symbol) val)
  (global sym)
  (push-set 'temp-vals sym)
  (set sym val))

(defn/c destroy-temps ()
  (map unbind (filter bound? temp-vals))
  (setq temp-vals nil))


(macro progn (BODY)
 `(let () ,@BODY))


(defn/c acons (key val alst)
  (cons (cons key val) alst))

(defn/c remove-if (lat pred)
  (let ((p pred))
    (filter (lambda (e)
              (not (p e)))
            lat)))

(defn/c remove (lat elem)
  (remove-if lat (equalto? elem)))

(defn/c assoc (k alst)
  (get (filter (lambda (v)
                 (equal (car v) k))
               alst)
       0))

(defn/c lookup (key alst)
  (let ((kvp (assoc key alst)))
    (if kvp (cdr kvp))))

(defn/c insert (elem lat pos)
  (append (slice lat 0 pos) (cons elem (slice lat pos))))

(defn append (lat1 lat2)
  ;; Not the most efficient way to implement append, but this implementation
  ;; with unquote-splicing is quite compact.
  `(,@lat1 ,@lat2))

(defn/c push ((sym . symbol) val)
  (set sym (cons val (eval sym))))


(defn/c push-set (sym val)
  (let ((tmp (cons val (eval sym))))
    (set sym (union tmp tmp))))


;; While suboptimal, these functions have the benefit of being small.
(defn/c min (lat) (car (sort lat <)))
(defn/c max (lat) (car (sort lat >)))

(defn/c replace (lat pred newv)
  ;; Note: The interpreter doesn't support capturing an enclosing function's
  ;; arguments, hence the let binding. Obviously, this is inconvenient in some
  ;; cases, but it's not that bad.
  (map (lambda (v)
         (if (pred v)
             newv
             v))
       lat))

(defn/c curry ((fn . lambda))
  (let ((func fn)
        (args (cdr $V)))
    (lambda ()
      (apply func (append args $V)))))

;; Return a predicate that returns true if its argument equals the supplied value.
;; e.g.: ((equalto? 2) 2) -> true
(defn/c equalto? (pred)
  (curry equal pred))

;; As useful as an equalto? predicate is, often you want to know if an element
;; of a sublist is equalto a value.
(defn/c pos-equalto? (pos pred)
  (let ((p pred)
        (n pos))
    (lambda (lat)
      (equal p (get lat n)))))

(defn/c car-equalto? (v)
  (let ((val v))
    (pos-equalto? 0 v)))

(defn/c notequal? (val)
  (let ((v val))
    (lambda (o)
      (not (equal o v)))))


(defn/c file-read (file offset len)
  (buffer-read (get (unwrap file) 2) offset len))

(defn/c file-size (file)
  (get (unwrap file) 1))
