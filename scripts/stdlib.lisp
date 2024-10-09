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
 `(map (fn ,@BODY) (range 0 ,N)))


;; Some useful macros for defining functions

;; The new defn macros, to replace the old ones using positional args.
(macro defn (NAME REST) `(setfn ,(cons $q NAME) (lambda ,@REST)))

;; Defines a bytecode-compiled function.  You should only compile long-lived
;; functions, because bytecode cannot be deallocated. At the same time, bytecode
;; takes up less space than non-compiled functions.
(macro defn/c (NAME REST) `(setfn ,(cons $q NAME) (compile (lambda ,@REST))))


(macro += (NAME VAL)
 `(setq ,NAME (+ ,NAME ,@VAL)))

(macro setq (NAME EXPR) `(set ,(cons $q NAME) ,@EXPR))

(macro when (EXPR BODY) `(if ,EXPR (progn ,@BODY)))
(macro unless (EXPR BODY) `(if (not ,EXPR) (progn ,@BODY)))

(global 'setfn)
(setq setfn
      (compile
       (lambda (sym fn)
         (global sym)
         (set sym fn))))


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
  (let ((temp k))
    (get (filter (lambda (v)
                   (equal (car v) temp))
                 alst)
         0)))

(defn/c lookup (key alst)
  (let ((kvp (assoc key alst)))
    (if kvp (cdr kvp))))

(defn/c insert (elem lat pos)
  (append (slice lat 0 pos) (cons elem (slice lat pos))))

(defn append (lat1 lat2)
  ;; Not the most efficient way to implement append, but this implementation
  ;; with unquote-splicing is quite compact.
  `(,@lat1 ,@lat2))

(defn/c gen (func n)
  (map func (range n)))

(defn/c push (sym val)
  (set sym (cons val (eval sym))))


(defn/c push-set (sym val)
  (let ((tmp (cons val (eval sym))))
    (set sym (union tmp tmp))))


(defn/c merge (l1 l2 comp)
  (cond
   ((not l1) l2)
   ((not l2) l1)
   ((comp (car l1) (car l2))
    (cons (car l1) ((this) (cdr l1) l2 comp)))
   (true (cons (car l2) ((this) l1 (cdr l2) comp)))))


(defn/c sort (lat comp)
  (if (not (cdr lat))
      lat
    (let ((len (length lat)))
      (merge ((this) (slice lat 0 (/ len 2)) comp)
             ((this) (slice lat (/ len 2)) comp)
             comp))))

;; While suboptimal, these functions have the benefit of being small.
(defn/c min (lat) (car (sort lat <)))
(defn/c max (lat) (car (sort lat >)))

(defn/c replace (lat p n)
  ;; Note: the interpreter doesn't support capturing an enclosing function's
  ;; arguments, hence the let binding. Obviously, this is inconvenient in some
  ;; cases, but it's not that bad.
  (let ((pred p)
        (newv n))
    (map (lambda (v)
          (if (pred v)
              newv
              v))
         lat)))

(defn/c curry (fn)
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
  (buffer-read (get file 2) offset len))

(defn/c file-size (file)
  (get file 1))
