;;;
;;; init.lisp
;;;


(when (is-developer-mode)
  (strict-mode true)
  (lisp-mem-crit-gc-alert true))


(lisp-mem-set-gc-thresh default-early-gc-thresh)


;; Define some common global
;; variables.
(eval-file "/scripts/globals.lisp")

;; Let's define some useful
;; builtin functions:

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
(defn/c dialog-setup-y/n ()
  (dialog-setup-binary-q "yes" "no"))

;; NOTE: These two functions defined as non-bytecode-complied to allow nested
;; functions on-dialog-accpted/declined to use await syntax.
(defn --try-dialog-accept () (if on-dialog-accepted (on-dialog-accepted)))
(defn --try-dialog-decline () (if on-dialog-declined (on-dialog-declined)))


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

;; NOTE: a bytecode compiled function cannot call another compiled function that
;; calls await, so dialog-await-y/n is currently interpreted. I will fix this
;; someday. If you aren't an expert in the scripting language, just avoid defn/c
;; and you should be fine.
(defn dialog-await-y/n ((text . string))
  (dialog-await-binary-q text "yes" "no"))


;; shortcut accessors for room metadata
(defn/c rinfo (key sym)
  (lookup key (room-meta sym)))


;; Shortcut for making sure enough space on a player's island exists to place a
;; new block.
(defn/c alloc-space ((sym . symbol))
  (let ((size (rinfo 'size sym)))
    (while (not (construction-sites (player) size))
      (terrain-set (player) (+ (terrain (player)) 1)))))


(defn/c hash (v)
  (cond
    ((and (pair? v) (int? (car v)) (int? (cdr v))) ;; hash for xy coord pair
     (let ((h (+ (* (first v) 374761393) (* (second v) 668265263))))
       (abs (* (bit-xor h (bit-shift-right h 13)) 1274126177))))
    (true
     (error (format "cannot hash %" v)))))


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

(global 'floor)
(setq floor int) ; cast to int rounds down

(defn/c push-pending-event ((turns . int) (script . string))
  (setq pending-events (cons (cons turns script) pending-events)))


(setvar "enabled_factions_bitfield"
        (bit-or faction-enable-human-mask
                faction-enable-goblin-mask
                faction-enable-sylph-mask))

;; The autoload mechanism provides a way to lazy-load infrequently used
;; symbols. As a final step before raising an undefined variable error, the
;; interpreter calls on-autoload for a symbol, to attempt to lazy-bind a value
;; from a file.
(defn/c --on-autoload (sym)
  (if (int? (find sym --autoload-symbols))
      (set-temp sym (eval-file (string "/scripts/autoload/" sym ".lisp")))))
