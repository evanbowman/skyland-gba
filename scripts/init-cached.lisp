;;;
;;; init-cached.lisp
;;;
;;; NOTE: these frequently-used functions are passed through an optimizing
;;; bytecode compiler at startup, and their compiled representations are
;;; compressed and cached in save memory for subsequent boots.
;;;


(defn/c clamp (v low high)
  (cond
   ((< v low) low)
   ((> v high) high)
   (true v)))

(defn/c procgen ()
  (opponent-generate (case (zone)
                       (0 (clamp (- (length enemies-seen) 1) 0 3))
                       (1 5)
                       (2 12)
                       (else 16))))

(defn/c zone ()
  (car (wg-pos)))

;; Choose a random element of a list.
(defn/c sample ((lat . pair))
  (get lat (choice (length lat))))

(defn/c secret ((x . int) (y . int) (text . string))
  (room-mut (opponent) x y 'code)
  (qr-set (opponent) x y text))

;; NOTE: see adventure_log.txt for message text...
(defn/c adventure-log-add ((id . int) (args . pair))
  ;; args: event-code parameters
  (setq adventure-log (cons (cons id args) adventure-log)))


(defn/c dialog-opts-reset ()
  (setq dialog-opts nil))


(defn/c dialog-opts-push ((name . string) (cb . lambda))
  (setq dialog-opts (cons (cons name cb) dialog-opts)))


;; For backwards compatibility. The old dialog api had a function for setting up
;; a yes/no question box, and the engine would then invoke on-dialog-accepted
;; and on-dialog-declined callbacks. But, eventually, I wanted more control over
;; the dialog options, and to allow players to select from more than two
;; options. This helper function exists for when only a simple yes/no choice is
;; needed, but the dialog-opts-reset and dialog-opts-push functions may be
;; called manually for more fine-grained control over dialog settings.
(defn/c dialog-setup-y/n ()
  (dialog-setup-binary-q "yes" "no"))


(defn/c dialog-setup-binary-q ((txt1 . string) (txt2 . string))
  (dialog-opts-reset)
  (dialog-opts-push txt1 --try-dialog-accept)
  (dialog-opts-push txt2 --try-dialog-decline))


;; This fairly niche function opens a box of options. The first option being the
;; yes option, the last option being the no option. Sandwitched in between, will
;; be a bunch of worldbuilding questions. If you select a middle option, the
;; game will show the text, and then re-display the query box of options, with
;; the previously selected one removed.
(defn/c dialog-await-binary-q-w/lore ((message . string)
                                      (texty . string)
                                      (textn . string)
                                      lore)
  (let ((result nil)
        (msg message)
        (lore-opts lore))
    (while (nil? result)
      (let ((sel (await (dialog-choice* msg (flatten (list texty
                                                           (map car lore-opts)
                                                           textn))))))
        (case sel
          (0
           (setq result 1))
          ((incr (length lore-opts))
           (setq result 0))
          (else
           (let ((opt (get lore-opts (decr sel))))
             (setq msg (cdr opt))
             (setq lore-opts (remove lore-opts opt)))))))
    result))



(defn/c dialog-choice* ((text . string) choices)
  (dialog-opts-reset)
  (foreach (lambda (c)
             (dialog-opts-push c (lambda () nil)))
           choices)
  (dialog* text))


(defn/c dialog-await-binary-q ((text . string) y n)
  (equal 0 (await (dialog-choice* text (list y n)))))

;; shortcut accessors for room metadata
(defn/c rinfo (key sym)
  (lookup key (room-meta sym)))


;; Shortcut for making sure enough space on a player's island exists to place a
;; new block.
(defn/c alloc-space ((sym . symbol))
  (let ((size (rinfo 'size sym)))
    (while (not (construction-sites (player) size))
      (terrain-set (player) (+ (terrain (player)) 1)))))


(defn/c chance ((n . int)) ; 1 in n chance
  (equal 0 (choice n)))


(defn/c sleep (time-ms)
  (await (wait* time-ms)))


(defn/c load-commentary ((key . string))
  (let ((search key)
        (opts nil))
    (foreach (lambda (chr)
               (let ((icon (lookup 'icon (cddr chr))))
                 (when icon
                   (let ((text (read-ini "/scripts/data/character_inter.ini"
                                         (format "character_%" icon)
                                         search)))
                     (when text
                       (setq opts (cons text opts)))))))
             (chrs (player)))
    (if opts
        (sample opts)
        nil)))

;; Exit a level, with a character on your crew making a comment about what
;; happened in the level. Key should be a possible key in character_inter.ini.
(defn/c exit-with-commentary ((key . string))
  (let ((message (load-commentary key)))
    (when message
      (dialog message)
      (setq on-dialog-closed nil))
    (exit)))


(defn/c push-pending-event ((turns . int) (script . string))
  (setq pending-events (cons (cons turns script) pending-events)))

(defn/c configure-vars ((vlat . pair))
  (let ((sv setvar)
        (l vlat))
    (while l
      (let ((kvp (car l)))
        (sv (cdr kvp) (car kvp)))
      (setq l (cdr l)))))


;; Some bytecode optimized functions used in quest marker computation:
(defn/c quest-marker-can-place (node)
  (let ((type (car node)))
    (and (not (equal type 0))
         (not (equal type 4))
         (not (equal type 5))
         (not (equal (cdr node) (cdr (wg-pos)))))))


(defn/c quest-marker-is-reachable (pos)
  (let ((pos-xy (cdr pos)))
    (lambda (n)
      (let ((xy (cdr n)))
        (let ((turns-until-corrupted (wg-turns-remaining xy)))
          ;; NOTE: +1 because path includes both endpoints.
          (> (+ turns-until-corrupted 1) (length (wg-path pos-xy xy))))))))


(defn/c sky-chart-xsort-compare (node1 node2)
  (let ((getx cadr))
    (> (getx node1) (getx node2))))


;; The autoload mechanism provides a way to lazy-load infrequently used
;; symbols. As a final step before raising an undefined variable error, the
;; interpreter calls on-autoload for a symbol, to attempt to lazy-bind a value
;; from a file.
(defn/c --on-autoload (sym)
  (if (int? (find sym --autoload-symbols))
      (set-temp sym (eval-file (string "/scripts/autoload/" sym ".lisp")))))
