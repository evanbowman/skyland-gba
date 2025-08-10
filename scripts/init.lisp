;;;
;;; init.lisp
;;;


(when (is-developer-mode)
  (strict-mode true)
  (lisp-mem-crit-gc-alert true))


;; NOTE: Based on some testing (see the game's syslog),
;; SKYLAND rarely ever has more than about 2000 LISP values
;; allocated at a time. When we're down to 3000 out of
;; 10k available vals seems like a reasonable time to
;; run it early. We could wait until we're completely out
;; of memory, but the interpreter is a bit buggy in that
;; state...
(lisp-mem-set-gc-thresh 3000)


;; Define some common global
;; variables.
(eval-file "/scripts/globals.lisp")

;; Let's define some useful
;; builtin functions:

(defn/c cargo-bays ((isle . wrapped))
  (let ((rooms-list (rooms isle)))
    (map (lambda (room)
           (cons (cadr room)
                 (cadr (cdr room))))
         (filter (car-equalto? 'cargo-bay) rooms-list))))

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
(defn/c sample ((lat . pair))
  (get lat (choice (length lat))))

(defn/c secret ((x . int) (y . int) (text . string))
  (room-mut (opponent) x y 'code)
  (qr-set (opponent) x y text))

;; NOTE: see adventure_log.txt for message text...
(defn/c adventure-log-add ((id . int) (args . pair))
  ;; args: event-code parameters
  (setq adventure-log (cons (cons id args) adventure-log)))


(global 'dialog-opts)

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
(defn/c dialog-await-y/n ()
  (dialog-await-binary-q "yes" "no"))

(defn/c dialog-await-binary-q ((txt1 . string) (txt2 . string))
  (dialog-opts-reset)
  (dialog-opts-push txt1 (lambda () (if on-dialog-accepted (on-dialog-accepted))))
  (dialog-opts-push txt2 (lambda () (if on-dialog-declined (on-dialog-declined)))))


;; This fairly niche function opens a box of options. The first option being the
;; yes option, the last option being the no option. Sandwitched in between, will
;; be a bunch of worldbuilding questions. If you select a middle option, the
;; game will show the text, and then re-display the query box of options, with
;; the previously selected one removed.
(defn/c dialog-await-binary-q-w/lore ((txty . string) (txtn . string) lore)
  (dialog-opts-reset)
  (dialog-opts-push txty (lambda () (if on-dialog-accepted (on-dialog-accepted))))

  (let ((lr lore)
        (ty txty)
        (tn txtn)
        (t (this))) ; note: re-invoke current function.
    (foreach (lambda (kvp)
               (let ((k kvp))
                 (dialog-opts-push (first kvp)
                                   (let ((str (second kvp)))
                                     (lambda ()
                                       (t ty tn (remove lr k))
                                       (dialog str))))))
             lore))

  (dialog-opts-push txtn (lambda () (if on-dialog-declined (on-dialog-declined)))))


;; shortcut accessors for room metadata
(defn/c rinfo (key sym)
  (lookup key (room-meta sym)))


;; Shortcut for making sure enough space on a player's island exists to place a
;; new block.
(defn/c alloc-space ((sym . symbol))
  (let ((size (rinfo 'size sym)))
    (while (not (construction-sites (player) size))
      (terrain-set (player) (+ (terrain (player)) 1)))))

(defn/c run-util-script ((file . string))
  (let ((varg (cdr $V)))
    (apply (eval-file (string "/scripts/util/" file ".lisp"))
           varg)))


(defn/c hash (v)
  (cond
    ((and (pair? v) (int? (car v)) (int? (cdr v))) ;; hash for xy coord pair
     (let ((h (+ (* (first v) 374761393) (* (second v) 668265263))))
       (abs (* (bit-xor h (bit-shift-right h 13)) 1274126177))))
    (true
     (error (format "cannot hash %" v)))))


(defn/c chance ((n . int)) ; 1 in n chance
  (equal 0 (choice n)))


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


(setvar "enabled_factions_bitfield"
        (bit-or
         (bit-shift-left 1 0) ; human
         (bit-shift-left 1 1) ; goblin
         ;; (bit-shift-left 1 2) ; sylph
         ))
