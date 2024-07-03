;;;
;;; init.lisp
;;;

;; Define some common global
;; variables.

(eval-file "/scripts/globals.lisp")

;; Let's define some useful
;; builtin functions:

(defn/c cargo-bays [1]
  (let ((rooms (rooms $0)))
    (map
     (lambda
       (cons
        (cadr $0)
        (cadr (cdr $0))))
     (filter
      (lambda (equal (car $0) 'cargo-bay))
      rooms))))

(defn/c clamp [3]
  (cond
   ((< $0 $1) $1)
   ((> $0 $2) $2)
   (true $0)))

(defn/c procgen [0]
  (opponent-generate
   (cond
    ((equal (zone) 0)
     (clamp (- (length enemies-seen) 1) 0 3))
    ((equal (zone) 1) 5)
    ((equal (zone) 2) 12)
    (true 16))))

(defn/c zone [0]
  (car (wg-pos)))

;; Choose a random element of a list.
(defn/c sample [1]
  (get $0 (choice (length $0))))

(defn/c secret [3]
  (room-mut (opponent) $0 $1 'code)
  (qr-set (opponent) $0 $1 $2))

;; NOTE: see adventure_log.txt for message text...
(defn/c adventure-log-add [2]
  ;; args: event-code parameters
  (setq adventure-log (cons (cons $0 $1) adventure-log)))


(global 'dialog-opts)

(defn/c dialog-opts-reset [0]
  (setq dialog-opts nil))

(dialog-opts-reset)

(defn/c dialog-opts-push [2]
  (setq dialog-opts (cons (cons $0 $1) dialog-opts)))

;; For backwards compatibility. The old dialog api had a function for setting up
;; a yes/no question box, and the engine would then invoke on-dialog-accepted
;; and on-dialog-declined callbacks. But, eventually, I wanted more control over
;; the dialog options, and to allow players to select from more than two
;; options. This helper function exists for when only a simple yes/no choice is
;; needed, but the dialog-opts-reset and dialog-opts-push functions may be
;; called manually for more fine-grained control over dialog settings.
(defn/c dialog-await-y/n [0]
  (dialog-await-binary-q "yes" "no"))

(defn/c dialog-await-binary-q [2]
  (dialog-opts-reset)
  (dialog-opts-push $0 (lambda (if on-dialog-accepted (on-dialog-accepted))))
  (dialog-opts-push $1 (lambda (if on-dialog-declined (on-dialog-declined)))))

;; For backwards compatibility...
(defn/c repl [0] (push-menu "repl" '()))

;; shortcut accessors for room metadata
(defn/c rinfo [2]
  (lookup $0 (room-meta $1)))


;; Shortcut for making sure enough space on a player's island exists to place a
;; new block.
(defn/c alloc-space [1]
  (let ((size (rinfo 'size $0)))
    (while (not (construction-sites (player) size))
      (terrain-set (player) (+ terrain (player) 1)))))

(defn/c run-util-script [1]
  (let ((file $0)
        (varg (cdr $V)))
    (apply (eval-file (string "/scripts/util/" file))
           varg)))

;; Higher level dialog API (for future localization purposes)
(defn/c get-dialog [2] ; (ini-sector ini-key)
  (read-ini (string "/strings/dialog/" (lang) ".ini")
            $0
            $1))

(defn/c load-dialog [2] ; (ini-sector ini-key substitution-args...)
  (let ((str (get-dialog $0 $1))
        (va $V))
    (if (> (length va) 2)
        (setq str (apply format (cons str (cddr va)))))
    (dialog str)))
