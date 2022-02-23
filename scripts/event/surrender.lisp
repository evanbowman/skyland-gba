;;;
;;; surrender.lisp
;;;


(dialog
 "<c:goblin pirates:2>We surrender! Honesst, we promise not to pillage any other cassstles!")

(setq on-dialog-closed
      (lambda
        (setq on-dialog-closed '())
        (dialog "Destroying or plundering may offer more coins, but the pirates offer you -- as payment. Accept the pirates' surrender?")))
