(eval-file "/scripts/reset_hooks.lisp")


(defn/temp rcv-settings (settings)
  (dialog "Save new controls?")
  (dialog-await-y/n)
  (defn on-dialog-accepted ()
    (show-opts))
  (defn on-dialog-declined ()
    (show-opts)))


(defn/temp show-opts ()
  (dialog-opts-reset)
  (dialog "What would you like to do?")

  ;; Note: button remapping disabled on gba. There are only ten buttons...
  (when (not (equal (device-info 'name) "GameboyAdvance"))
    (dialog-opts-push "View Controls"
                      (lambda ()
                        (push-menu "rebind-buttons" '(0))
                        (show-opts)
                        (push-menu "dialog-chain" '())))

    (dialog-opts-push "Edit Controls"
                      (lambda ()
                        (push-menu "rebind-buttons" '(1))
                        (setq on-menu-resp rcv-settings)
                        (push-menu "dialog-chain" '()))))


  (let ((rumble (lookup 'rumble (settings-load))))
    (dialog-opts-push (if (equal rumble "on")
                          "Disable Rumble"
                          "Enable Rumble")
                      (lambda ()
                        (setq rumble (if (equal rumble "on") "off" "on"))

                        (let ((s (settings-load)))
                          (let ((old (assoc 'rumble s)))
                            (setq s (replace s (equalto? old) (cons 'rumble rumble)))
                            (settings-save s)))

                        (dialog "Rumble "
                                (if (equal rumble "on")
                                    "enabled!"
                                    "disabled!"))
                        (defn on-dialog-closed ()
                          (setq on-dialog-closed nil)
                          (show-opts)))))


  (dialog-opts-push "Exit"
                    (lambda ()
                      (push-menu "title-screen" '(3)))))


(show-opts)
