
(let ((content (eval-file "extracted_cart_ram/sb0.lisp")))
  (print (cdr (cdr (assoc 'rooms content)))))

(newline)
