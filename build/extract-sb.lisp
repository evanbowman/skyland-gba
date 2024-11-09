
(let ((content (eval-file "extracted_cart_ram/save/sb0.lisp")))
  (print (cdr (cdr (assoc 'rooms content)))))

(newline)
