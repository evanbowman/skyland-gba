
(put ">> ")
(set 'line (getline))

(while line
  (when (length line)
    (print (eval (read line)))
    (newline))
  (put ">> ")
  (set 'line (getline)))

(newline)
