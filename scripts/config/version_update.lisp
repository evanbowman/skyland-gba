;;;
;;; version_update.lisp
;;;
;;; This script runs when the game detects that the player updated their rom to
;;; a new version of the game. Here, we can make any necessary compatibility
;;; edits if we changed the format of any save data.
;;;

;; Erase old unused bytecode caches saved by previous versions.
(file-unlink "/bytecode/init.slb")
(file-unlink "/bytecode/stdlib.slb")


(defn repr-version (vn)
  (string-join vn "."))


(defn version-less (a b)
  (< (lexicographical-compare a b) 0))


(when-let ((vn (read-version-file "/save/version.dat")))

  ;; NOTE: make any changes needed here...

  (log (string "Updated from " (repr-version vn)
               " to " (repr-version (version))
               "!")))


(eval-file "/scripts/config/store_version.lisp")

(unbind 'repr-version
        'version-less)
