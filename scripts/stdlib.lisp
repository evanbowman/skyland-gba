;;;
;;; stdlib.lisp
;;;
;;; A small standard library
;;;


(macro or (expr)
 (if expr
     `(if ,(car expr)
          1
        ,(if (cdr expr)
             (cons 'or (cdr expr))
           0))
   0))


(macro and (expr)
 (if expr
     `(if (not ,(car expr))
          0
        ,(if (cdr expr)
             (cons 'and (cdr expr))
           1))
   1))


;; TODO: remove builtin progn function, use a macro instead.
;; (macro progn (body) `(let () ,@body))


(set 'acons
     (compile
      (lambda
        (cons (cons $0 $1) $2))))


(set 'assoc
     (compile
      (lambda
        (let ((temp $0))
          (get (filter
                (lambda (equal (car $0) temp))
                $1)
               0)))))


(set 'append (lambda `(,@$0 ,@$1)))


(set 'bisect
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


(set 'merge
     (compile
      (lambda
        (if (not $0)
            $1
          (if (not $1)
              $0
            (if (< (car $0) (car $1))
                (cons (car $0) ((this) (cdr $0) $1))
              (cons (car $1) ((this) $0 (cdr $1)))))))))


(set 'sort
     (lambda
       (if (not (cdr $0))
           $0
         (let ((temp (bisect $0)))
           (merge (sort (car temp))
                  (sort (cdr temp)))))))
