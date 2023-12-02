
(put ">> ")
(setq line (getline))

(while line
  (print (eval (read line)))
  (newline)
  (put ">> ")
  (setq line (getline)))
