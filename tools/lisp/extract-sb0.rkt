#lang racket

(define (read-lisp-file filename)
  (let ((input-port (open-input-file filename)))
    (let loop ((result '()))
      (let ((datum (read input-port)))
        (if (eof-object? datum)
            (begin
              (close-input-port input-port)
              (reverse result))
            (loop (cons datum result)))))))

(let ((data (car (read-lisp-file "extracted_cart_ram/save/sb0.lisp"))))
  (print (cdr (cdr (assoc 'rooms (car (cdr data))))))
  (newline))
