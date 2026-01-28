;;; This script accepts a variadic number of arguments. Pass a sequence of dialog
;;; strings and functions, and this script will create dialog boxes and invoke
;;; the functions in the order that they appear. For example:
;;;
;;; (run-util-script "dialog_sequence"
;;;                  "Dialog message one."
;;;                  (lambda ()
;;;                    do something after message one...)
;;;                  "Dialog message two."
;;;                  "Message three"
;;;                  (lambda ()
;;;                    ultimately, run this function...))
;;;
;;; The dialog sequence script is intended to make it easier to create simple
;;; dialog sequences without needing to chain together callbacks.

(lambda ()
  (let ((t (this))
        (args $V))
    (cond
      ((lambda? (car args))
       ((car args))
       (apply t (cdr args)))
      ((string? (car args))
       (dialog (car args))
       (defn on-dialog-closed ()
         (setq on-dialog-closed nil)
         (if (cdr args)
             (apply t (cdr args))))))))
