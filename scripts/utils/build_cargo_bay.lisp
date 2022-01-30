;;
;; This script accepts one parameter, a function, which it invokes with a list
;; of the player's empty cargo-bays, after waiting for the player to construct
;; a cargo-bay, or jettison some cargo.
;;
;; To invoke: (example)
;;
;; ((eval-file "/scripts/util/build_cargo_bay.lisp")
;;  (lambda <your code...>))
;;

(lambda
  (let ((temp $0))
    ((lambda
       (let ((retry (this))
             ;; filter cargo bays with no cargo
             (v (filter
                 (lambda (not (cargo (player) (get $0 1) (get $0 2))))
                 (cargo-bays (player)))))
         (if v
             (temp v)
           (progn
             (dialog "You have no empty cargo bays, want some time to build one now?")
             (dialog-await-y/n)
             (setq on-dialog-accepted
                   (lambda
                     (task 30000 retry)))
             (setq on-dialog-declined exit-level))))))))
