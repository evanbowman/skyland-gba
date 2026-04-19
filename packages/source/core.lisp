;;;
;;; core.lisp
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

(defn/c contains (lat entry)
  (int? (find entry lat)))

(defn/c remove (lat elem)
  (remove-if lat (equalto? elem)))

(defn/c assoc (filter-key alst)
  (get (filter (lambda (v)
                 (equal (car v) filter-key))
               alst)
       0))

(defn/c lookup (key alst)
  (when-let ((kvp (assoc key alst)))
    (cdr kvp)))

(defn/c insert (elem lat (pos . int))
  (append (slice lat 0 pos) (cons elem (slice lat pos))))


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
(defn/c pos-equalto? (position pred)
  (lambda (lat)
    (equal pred (get lat position))))

(defn/c clamp (v low high)
  (cond
   ((< v low) low)
   ((> v high) high)
   (true v)))

(defn/c car-equalto? (v)
  (pos-equalto? 0 v))

(defn/c notequal? (value)
  (lambda (o)
    (not (equal o value))))

(defn/c string-join (lat delim)
  (apply string (cdr (flatten (map (curry list delim) lat)))))

(defn/c ends-with ((str . string) (suffix . string))
  (let ((m1 (string-explode str))
        (m2 (string-explode suffix)))
    (equal (slice m1 (- (length m1) (length m2))) m2)))

(defn/c file-read ((file . wrapped) (offset . int) (len . int))
  (buffer-read (get (unwrap file) 2) offset len))

(defn/c file-size ((file . wrapped))
  (get (unwrap file) 1))

(defn/c file-to-ascii ((file . wrapped))
  (let ((bytes (file-read file 0 (file-size file))))
    (string-assemble bytes)))

(defn/c array-foreach ((cb . lambda) (ary . array))
  (let ((l (length ary))
        (i 0))
    (while (< i l)
      (cb (get ary i))
      (+= i 1))))

(defn/c lexicographical-compare (a b)
  (let ((min-len (min (list (length a) (length b)))))
    ((lambda (a b i)
       (if (equal i min-len)
           (cond ((< (length a) (length b)) -1)
                 ((> (length a) (length b))  1)
                 (true                       0))
           (let ((a-i (get a i))
                 (b-i (get b i)))
             (cond ((> b-i a-i) -1)
                   ((< b-i a-i)  1)
                   (true ((this) a b (incr i)))))))
     a b 0)))
