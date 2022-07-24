;;;
;;; Invoked when loading a saved macrocosm.
;;;

;; Inputs: bound to conf global variable by engine.


(defn hrs
  ; convert timestamp to hours
  (+ (* (get $0 0) 8760) ; hours per year (approx)
     (* (get $0 1) 730)  ; hours per month (approx)
     (* (get $0 2) 24)   ; hours per day
     (get $0 3)          ; hours
     ))


(let ((tm (assoc 'tm conf)))
  ;; If the game was saved on a cartridge with a realtime clock.
  (if tm
      (let ((now (syscall "startup-time"))
            (diff 0)
            (yrs 0)
            (year 0))
        (if now
            (cond
             ((equal now tm) '())
             (1
              (setq diff (- (hrs now) (hrs tm)))

              (if (> diff 0)
                  (setq yrs (diff / 8))
                (setq diff 0))

              (setq year (mcr-next yrs))

              (dialog
               (format "Welcome back! Time seems to pass more slowly while you're gone, % years have passed, and the current year is %. Why not check on your cities and make sure they're ok!" yrs year))))))))


(unbind 'conf 'hrs)
