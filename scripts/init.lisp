;;;
;;; init.lisp
;;;

(load-library "/packages/lib/core.slb")
(load-library "/packages/lib/util.slb")
(load-library "/packages/lib/locale.slb")


;; Convenience alias for translating text.
(defn/c tr (thing)
  (if tr-bindings
      (tr-load thing)
      thing))


(when (is-developer-mode)
  (strict-mode true)
  (lisp-mem-crit-gc-alert true))


(lisp-mem-set-gc-thresh default-early-gc-thresh)
(global 'dialog-opts)


;; Define some common global
;; variables.
(eval-file "/scripts/globals.lisp")

(if-let ((vn (read-version-file "/save/version.dat")))
    (when (not (equal vn (version)))
      (eval-file "/scripts/config/version_update.lisp"))
  (eval-file "/scripts/config/store_version.lisp"))


;; NOTE: These two functions defined as non-bytecode-complied to allow nested
;; functions on-dialog-accpted/declined to use await syntax.
(defn --try-dialog-accept () (if on-dialog-accepted (on-dialog-accepted)))
(defn --try-dialog-decline () (if on-dialog-declined (on-dialog-declined)))


;; NOTE: a bytecode compiled function cannot call another compiled function that
;; calls await, so dialog-await-y/n is currently interpreted. I will fix this
;; someday. If you aren't an expert in the scripting language, just avoid defn/c
;; and you should be fine.
(defn dialog-await-y/n ((text . string))
  (dialog-await-binary-q text (tr "yes") (tr "no")))


(engine-set "enabled_factions_bitfield"
            (bit-or faction-enable-human-mask
                    faction-enable-goblin-mask
                    faction-enable-sylph-mask))


(unbind 'load-library-cached)

(if (is-developer-mode)
    (progn
      (setq build-library (compile build-library))
      (eval-file "/scripts/test/boot-diagnostics.lisp"))
    (unbind 'build-library))
