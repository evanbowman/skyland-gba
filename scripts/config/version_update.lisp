;;;
;;; version_update.lisp
;;;
;;; This script runs when the game detects that the player updated their rom to
;;; a new version of the game. Here, we can make any necessary compatibility
;;; edits if we changed the format of any save data.
;;;

;; Erase old unused bytecode caches saved by previous versions.
(file-unlink "/bytecode/init-cached.slb")
(file-unlink "/bytecode/stdlib-cached.slb")

(defn/temp repr-version (vn)
  (apply string (cdr (flatten (map (curry list ".") vn)))))


(defn/temp version-less (a b)
  (cond
    ((not a) (and b true))
    ((not b) false)
    ((< (car a) (car b)) true)
    ((> (car a) (car b)) false)
    (true (version-less (cdr a) (cdr b)))))


(when-let ((vn (read-version-file)))
  (let ((current (version)))

    ;; TODO: make any changes needed here...

    (log (string "Updated from " (repr-version vn)
                 " to " (repr-version (version))
                 "!"))))


(eval-file "/scripts/config/store_version.lisp")
