;;;
;;; init.lisp
;;;

;; Define some common global
;; variables.

(eval-file "/scripts/globals.lisp")

;; Let's define some useful
;; builtin functions:

(defn/c cargo-bays (isle)
  (let ((rooms (rooms isle)))
    (map (lambda (room)
           (cons (cadr room)
                 (cadr (cdr room))))
         (filter (car-equalto? 'cargo-bay) rooms))))

(defn/c clamp (v low high)
  (cond
   ((< v low) low)
   ((> v high) high)
   (true v)))

(defn/c procgen ()
  (opponent-generate
   (cond
    ((equal (zone) 0)
     (clamp (- (length enemies-seen) 1) 0 3))
    ((equal (zone) 1) 5)
    ((equal (zone) 2) 12)
    (true 16))))

(defn/c zone ()
  (car (wg-pos)))

;; Choose a random element of a list.
(defn/c sample (lat)
  (get lat (choice (length lat))))

(defn/c secret (x y text)
  (room-mut (opponent) x y 'code)
  (qr-set (opponent) x y text))

;; NOTE: see adventure_log.txt for message text...
(defn/c adventure-log-add (id args)
  ;; args: event-code parameters
  (setq adventure-log (cons (cons id args) adventure-log)))


(global 'dialog-opts)

(defn/c dialog-opts-reset ()
  (setq dialog-opts nil))

(dialog-opts-reset)

(defn/c dialog-opts-push (name cb)
  (setq dialog-opts (cons (cons name cb) dialog-opts)))

;; For backwards compatibility. The old dialog api had a function for setting up
;; a yes/no question box, and the engine would then invoke on-dialog-accepted
;; and on-dialog-declined callbacks. But, eventually, I wanted more control over
;; the dialog options, and to allow players to select from more than two
;; options. This helper function exists for when only a simple yes/no choice is
;; needed, but the dialog-opts-reset and dialog-opts-push functions may be
;; called manually for more fine-grained control over dialog settings.
(defn/c dialog-await-y/n ()
  (dialog-await-binary-q "yes" "no"))

(defn/c dialog-await-binary-q (txt1 txt2)
  (dialog-opts-reset)
  (dialog-opts-push txt1 (lambda () (if on-dialog-accepted (on-dialog-accepted))))
  (dialog-opts-push txt2 (lambda () (if on-dialog-declined (on-dialog-declined)))))

;; For backwards compatibility...
(defn/c repl () (push-menu "repl" '()))

;; shortcut accessors for room metadata
(defn/c rinfo (key sym)
  (lookup key (room-meta sym)))


;; Shortcut for making sure enough space on a player's island exists to place a
;; new block.
(defn/c alloc-space (sym)
  (let ((size (rinfo 'size sym)))
    (while (not (construction-sites (player) size))
      (terrain-set (player) (+ terrain (player) 1)))))

(defn/c run-util-script (file)
  (let ((varg (cdr $V)))
    (apply (eval-file (string "/scripts/util/" file))
           varg)))
