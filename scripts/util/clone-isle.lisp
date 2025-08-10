;;;
;;; util/clone-isle.lisp
;;;


(lambda (src dest)
  (let ((t (terrain src))
        (d dest))
    (terrain-set dest t)
    (island-configure dest
                      (map (lambda (room)
                             (let ((size-xy (rinfo 'size (car room))))
                               (list (car room)
                                     (- t (+ (first size-xy)
                                             (get room 1)))
                                     (get room 2))))
                           (rooms src)))
    (foreach (lambda (chr)
               (chr-new d
                        (- (- t 1) (car chr))
                        (get chr 1)
                        (if (equal d (player))
                            'neutral
                            'hostile)
                        nil))
             (chrs src))))
