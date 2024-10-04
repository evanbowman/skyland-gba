
(lambda (src dest)
  (let ((t (terrain src))
        (d dest))
    (terrain-set dest t)
    (island-configure dest
                      (map (lambda (r)
                             (let ((size (rinfo 'size (car r))))
                               (list (car r)
                                     (- t (+ (car size)
                                             (get r 1)))
                                     (get r 2))))
                           (rooms src)))
    (foreach (lambda (c)
               (chr-new d
                        (- (- t 1) (car c))
                        (get c 1)
                        (if (equal d (player))
                            'neutral
                            'hostile)
                        nil))
             (chrs src))))
