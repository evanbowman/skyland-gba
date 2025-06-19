
(defn/temp gen (cb n)
  (let ((i 0)
        (ret nil))
    (while (< i n)
      (setq ret (cons (cb) ret))
      (+= i 1))
    ret))

(defn/temp shuffle (lat)
  (let ((tmp (map cons lat (gen (curry choice 10000) (length lat)))))
    (setq tmp (sort tmp (lambda (v1 v2)
                          (< (cdr v1) (cdr v2)))))
    (map car tmp)))


(defn/temp get-icon (chr)
  (lookup 'icon (cddr chr)))


(lambda (k)
  (let ((crew (filter get-icon (chrs (player))))
        (key k))
    (setq crew (map get-icon (slice (shuffle crew) 0 (+ 1 (choice 3)))))
    ((lambda ()
       (if crew
           (progn
             (dialog (read-ini "/scripts/data/character_inter.ini"
                               (format "character_%" (car crew))
                               key))
             (setq crew (cdr crew))
             (setq on-dialog-closed (this)))
           (setq on-dialog-closed exit))))))
