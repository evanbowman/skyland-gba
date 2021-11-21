;;;
;;; init.lisp
;;;


(eval-other-file "stdlib.lisp")


(def language 'english)

(defn/c locale-string
  (get-line-of-file (string "strings/" language '.txt) $0))
