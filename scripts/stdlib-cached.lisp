;;;
;;; stdlib-cached.lisp
;;;


(defn/c set-temp ((sym . symbol) val)
  (global sym)
  (push-set 'temp-vals sym)
  (set sym val))

(defn/c destroy-temps ()
  (map unbind (filter bound? temp-vals))
  (setq temp-vals nil))

(defn/c acons (key val alst)
  (cons (cons key val) alst))

(defn/c remove-if (lat pred)
  (filter (lambda (e)
            (not (pred e)))
          lat))

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


(defn/c push ((sym . symbol) val)
  (set sym (cons val (eval sym))))


(defn/c push-set (sym val)
  (let ((tmp (cons val (eval sym))))
    (set sym (union tmp tmp))))


(defn/c replace (lat pred newv)
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
