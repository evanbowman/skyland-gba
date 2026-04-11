;;;
;;; surrender/salvage_rights.lisp
;;;

(tr-bind-current)

(lambda (blocks)
  (let ((cnt blocks)
        (taken 0))
    (while cnt
      (let (([x . y] (await (sel-input-opponent* nil (format (tr "Take block: (%/%)") (- blocks cnt) blocks)))))
        (let ((took (car (room-load (opponent) x y))))
          (cond
            ((room-is-critical (opponent) x y)
             (when (dialog-await-y/n (tr "This will remove the island's only power source, causing it to become unstable (you won't be able to take any more blocks), are you sure?"))
               (sound "gravel")
               (alloc-space took)
               (let (([dx . dy] (await (sel-input* took (tr "Place block:")))))
                 (room-new (player) (list took dx dy))
                 (sound "build0")
                 (room-del (opponent) x y)
                 (+= taken 1)
                 (setq cnt 0))))
            ((not took)
             (sound "beep_error"))
            (true
             (room-del (opponent) x y)
             (sound "gravel")
             (alloc-space took)
             (let (([dx . dy] (await (sel-input* took (format (tr "Place block: (%/%)") (- blocks cnt) blocks)))))
               (room-new (player) (list took dx dy))
               (sound "build0")
               (setq cnt (decr cnt))
               (+= taken 1)))))))
    (await (dialog* (format (tr "Accepted surrender, and acquired % blocks!") taken)))
    (adventure-log-add 62 '())
    (exit 2)))
