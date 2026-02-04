(eval-file "/scripts/reset_hooks.lisp")


(global 'show-opts) ; For the linter

(defn/temp setup-keypad-opts ()
  (dialog-opts-push "View Controls"
                    (lambda ()
                      (push-menu "rebind-buttons" '(0))
                      (show-opts)
                      (push-menu "dialog-chain" '())))

  (dialog-opts-push "Edit Controls"
                    (lambda ()
                      (push-menu "rebind-buttons" '(1))
                      (setq on-menu-resp (lambda (settings)
                                           (dialog "Save new controls?")
                                           (dialog-setup-y/n)
                                           (defn on-dialog-accepted ()
                                             (settings-save settings)
                                             (show-opts))
                                           (defn on-dialog-declined ()
                                             (show-opts))))
                      (push-menu "dialog-chain" '())))

  (dialog-opts-push "Restore Defaults"
                    (lambda ()
                      (eval-file "/scripts/config/set_default_keys.lisp")
                      (show-opts))))


(defn/temp edit-setting (name val)
  (let ((s (settings-load)))
    (let ((old (assoc name s)))
      (setq s (if old
                  (replace s (equalto? old) (cons name val))
                  (cons (cons name val) s)))
      (settings-save s))))


(defn/temp show-opts ()
  (dialog-opts-reset)
  (dialog "What would you like to do?")

  (when (not (equal (device-info 'name) "GameboyAdvance"))
    (setup-keypad-opts)

    (let ((lighting (lookup 'lighting (settings-load))))
      (dialog-opts-push (if (equal lighting "on")
                            "Disable Lighting Effects"
                            "Enable Lighting Effects")
                        (lambda ()
                          (setq lighting (if (equal lighting "on") "off" "on"))
                          (edit-setting 'lighting lighting)
                          (dialog "Dynamic lighting graphical effects "
                                  (if (equal lighting "on")
                                      "enabled!"
                                      "disabled!"))
                          (defn on-dialog-closed ()
                            (setq on-dialog-closed nil)
                            (show-opts))))))

  (let ((rumble (lookup 'rumble (settings-load))))
    (dialog-opts-push (if (equal rumble "on")
                          "Disable Rumble"
                          "Enable Rumble")
                      (lambda ()
                        (setq rumble (if (equal rumble "on") "off" "on"))
                        (edit-setting 'rumble rumble)
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
