
(put ">> ")
(setq line (getline))

(while line
  (when (length line)
    (print (eval (read line)))
    (newline))
  (put ">> ")
  (setq line (getline)))

(newline)
