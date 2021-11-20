;;;
;;; init.lisp
;;;


(set 'language 'english)

(eval-other-file "stdlib.lisp")



(set 'locale-string
     (compile
      (lambda
        (get-line-of-file (string "strings/" language '.txt) $0))))
