;;;
;;; init.lisp
;;;
;;; Many of these functions run frequently, and are compiled to optimized
;;; bytecode at startup for greater compactness and better performance.
;;;

(when (is-developer-mode)
  ;; TODO: fix type checking for bytecode functions and re-enable strict mode.
  ;; (strict-mode true)
  ;; TODO: this one also needs more testing.
  ;;(lisp-mem-crit-gc-alert true)
  )


(lisp-mem-set-gc-thresh default-early-gc-thresh)
(global 'dialog-opts)


;; Define some common global
;; variables.
(eval-file "/scripts/globals.lisp")


(load-library-cached "/scripts/init-cached.lisp" "/bytecode/init.slb")


;; NOTE: These two functions defined as non-bytecode-complied to allow nested
;; functions on-dialog-accpted/declined to use await syntax.
(defn --try-dialog-accept () (if on-dialog-accepted (on-dialog-accepted)))
(defn --try-dialog-decline () (if on-dialog-declined (on-dialog-declined)))


;; NOTE: a bytecode compiled function cannot call another compiled function that
;; calls await, so dialog-await-y/n is currently interpreted. I will fix this
;; someday. If you aren't an expert in the scripting language, just avoid defn/c
;; and you should be fine.
(defn dialog-await-y/n ((text . string))
  (dialog-await-binary-q text "yes" "no"))


(setvar "enabled_factions_bitfield"
        (bit-or faction-enable-human-mask
                faction-enable-goblin-mask
                faction-enable-sylph-mask))


(unbind 'load-library-cached 'open-library-cached)
