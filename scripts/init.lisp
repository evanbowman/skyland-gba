;;;
;;; init.lisp
;;;

(eval-file "/scripts/stdlib.lisp")


;; Let's define some useful
;; builtin functions:

(defn/c cargo-bays
  (let ((rooms (rooms $0)))
    (map
     (lambda
       (cons
        (car (cdr $0))
        (car (cdr (cdr $0)))))
     (filter
      (lambda (equal (car $0) 'cargo-bay))
      rooms))))

(defn/c clamp
  (cond
   ((< $0 $1) $1)
   ((> $0 $2) $2)
   (true $0)))

(defn/c procgen
  (opponent-generate
   (cond
    ((equal (zone) 0)
     (clamp (- (length enemies-seen) 1) 0 3))
    ((equal (zone) 1) 5)
    ((equal (zone) 2) 12)
    (true 16))))

(defn/c zone
  (car (wg-pos)))

;; Choose a random element of a list.
(defn/c sample
  (get $0 (choice (length $0))))

(defn/c secret
  (room-mut (opponent) $0 $1 'code)
  (qr-set (opponent) $0 $1 $2))

;; NOTE: see adventure_log.txt for message text...
(defn/c adventure-log-add
  ;; args: event-code parameters
  (setq adventure-log (cons (cons $0 $1) adventure-log)))


(defn/c dialog-opts-reset
  (setq dialog-opts nil))

(dialog-opts-reset)

(defn/c dialog-opts-push
  (setq dialog-opts (cons (cons $0 $1) dialog-opts)))

;; For backwards compatibility. The old dialog api had a function for setting up
;; a yes/no question box, and the engine would then invoke on-dialog-accepted
;; and on-dialog-declined callbacks. But, eventually, I wanted more control over
;; the dialog options, and to allow players to select from more than two
;; options. This helper function exists for when only a simple yes/no choice is
;; needed, but the dialog-opts-reset and dialog-opts-push functions may be
;; called manually for more fine-grained control over dialog settings.
(defn/c dialog-await-y/n
  (dialog-await-binary-q "yes" "no"))

(defn/c dialog-await-binary-q
  (dialog-opts-reset)
  (dialog-opts-push $0 (lambda (if on-dialog-accepted (on-dialog-accepted))))
  (dialog-opts-push $1 (lambda (if on-dialog-declined (on-dialog-declined)))))

;; For backwards compatibility...
(defn/c repl (push-menu "repl" '()))

;; shortcut accessors for room metadata
(defn/c rinfo
  (cdr (assoc $0 (room-meta $1))))
