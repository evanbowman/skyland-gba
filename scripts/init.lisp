;;;
;;; init.lisp
;;;


(set 'language 'english)

(eval-other-file "stdlib.lisp")


(defn-c 'locale-string
  (get-line-of-file (string "strings/" language '.txt) $0))
