

(defn on-A [0]
  (gui-set-attr "t2" "val" "hello, world"))


(defn on-B [0]
  (push-menu "ready" '()))


(defn on-L [0]
  nil)


(defn on-R [0]
  nil)


(defn on-U [0]
  nil)


(defn on-D [0]
  nil)


(defn on-menu-exit [0]
  (unbind 'on-A 'on-B 'on-L 'on-R 'on-U 'on-D))
