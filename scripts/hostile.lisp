;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-other-file "reset_hooks.lisp")


(configure-player
 (player)
 '((stairwell 0 11)
   (infirmary 1 11)
   (workshop 3 13)
   (stairwell 3 9)
   (stairwell 4 9)
   (workshop 1 9)))


(init-opponent 3 'hostile)


(configure-player
 (opponent)
 '((missile-silo 0 13)
   (power-core 1 13)))


(add-hostile-chr (player) 1 10)
(add-hostile-chr (player) 2 10)
(add-hostile-chr (player) 3 11)


;; (if (not (equal (zone) last-zone))
;;     (progn
;;       (set 'friendlies-seen '())
;;       (set 'enemies-seen '())))


;; (set 'last-zone (zone))


;; (set 'avail-levels ;; list of unvisited levels
;;      (filter
;;       (lambda
;;         (set 'temp (arg 0))
;;         (not (length (filter
;;                       (lambda (equal temp (arg 0)))
;;                       enemies-seen))))
;;       (gen
;;        (get '(8 3) (zone)) ;; number of levels to select from based on current zone
;;        (lambda (arg 0)))))


;; ;; Ok, so if we're at the point where we've exhausted all of the possible level
;; ;; scenarios (which shouldn't really happen, anyway), we should clear the list
;; ;; of seen enemies, so that next time we won't end up with nil.
;; (if (equal (length avail-levels) 1)
;;     (set 'enemies-seen '()))


;; (set 'lv-num
;;      (if (equal (length enemies-seen) 0)
;;          0
;;        (get avail-levels (cr-choice (length avail-levels)))))


;; (set 'enemies-seen (cons lv-num enemies-seen))


;; (eval-other-file (string 'hostile '_ (zone) '_ lv-num '.lisp))


;; ;; Just to save some memory...
;; (set 'avail-levels '())
;; (gc)
