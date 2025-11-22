;;;
;;; sandbox.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")

;; NOTE: Engine binds config fields from the sandbox settings menu to a list
;; variable called conf.

(global 'conf) ;; Declaration for the linter.

(coins-set (get conf 0))
(terrain-set (player) (get conf 1))


(defn sb-help ()
  (dialog "Sandbox mode gives you nearly unlimited resources, and allows you to build on your opponent's island in addition to your own!<B:0> You may also reposition your opponent's characters!<B:0> Try out strategies, or just play around!<B:0> You can even build a couple of big fortresses, select spectate on the start menu, and let the AI control both castles!"))


(if (not (save-bit-load 3))
    (setq on-fadein
          (lambda ()
            (setq on-fadein nil)
            (save-bit-store 3 1)
            (dialog "Welcome to the Battle Sandbox! Want any help?")
            (dialog-await-binary-q "Sure!" "No thanks!")
            (setq on-dialog-accepted sb-help)
            (setq on-dialog-declined nil))))





;; NOTE: In case I haven't explained elsewhere, the interpreter does a small
;; symbol optimization to save space in the string intern table, hence all of
;; the four-character variable names.
(defn mkch (i m)
  ;; Arg 0: island
  ;; Arg 1: 'hostile or 'neutral symbol
  (let ((isle i)
        (mode m))

    ;; NOTE: conf[5] holds the character count config.
    (dotimes (get conf 5)
      (let ((slot (chr-slots isle)))
        (if (not slot)
            (let ((s (construction-sites isle '(2 . 2))))
              (if (not s)
                  (fatal "Not enough room to place chrs!"))
              (room-new isle (list 'workshop (caar s) (cdr (car s))))
              (setq slot (chr-slots isle))))
        (if slot
            (chr-new isle
                     (caar slot)
                     (cdr (car slot))
                     mode
                     nil))))))


(island-configure
 (player)
 '((power-core 1 13)))


(mkch (player) 'neutral)
(flag-show (player) 0)



(opponent-init (get conf 1) 'hostile)


(flag-show (opponent) flag-id-pirate)


(island-configure
 (opponent)
 `((power-core ,(- (get conf 1) 3) 13)))

(mkch (opponent) 'hostile)



(defn import-level ()
  (push-menu "file-browser" '("/scripts/event/" true))

  (defn on-menu-resp (path)
    (eval-file path)
    (dialog-reset)
    (island-set-pos (opponent) (+ 250 (* 16 (- 10 (terrain (opponent))))) 374)
    (eval-file "/scripts/reset_hooks.lisp")))



(unbind 'conf 'mkch)

(setvar "powerdown_allowed" 1)
(setvar "rewind_disabled" 0)
