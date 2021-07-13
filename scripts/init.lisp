;;;
;;; init.lisp
;;;


(set 'language 'english)


(set 'locale-string
     (lambda
       (get-line-of-file (string language '.txt))))
