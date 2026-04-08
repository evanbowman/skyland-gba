(eval-file "/scripts/reset_hooks.lisp")
(tr-bind-current)


(global 'show-opts) ; For the linter

(defn/temp setup-keypad-opts ()
  (dialog-opts-push (tr "View Controls")
                    (lambda ()
                      (push-menu "rebind-buttons" '(0))
                      (show-opts)
                      (push-menu "dialog-chain" '())))

  (dialog-opts-push (tr "Edit Controls")
                    (lambda ()
                      (push-menu "rebind-buttons" '(1))
                      (setq on-menu-resp (lambda (settings)
                                           (dialog (tr "Save new controls?"))
                                           (dialog-setup-y/n)
                                           (defn on-dialog-accepted ()
                                             (settings-save settings)
                                             (show-opts))
                                           (defn on-dialog-declined ()
                                             (show-opts))))
                      (push-menu "dialog-chain" '())))

  (dialog-opts-push (tr "Restore Defaults")
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
  (dialog (tr "What would you like to do?"))

  (let ((lang-opts (eval-file "/strings/lang.lisp")))
    (dialog-opts-push (tr "Change Language")
                      (lambda ()
                        (dialog-opts-reset)
                        (foreach (lambda (opt)
                                   (dialog-opts-push
                                    (car opt)
                                    (lambda ()
                                      (dialog-setup-y/n)
                                      (dialog (format (tr "Really change language to '%'?")
                                                      (car opt)))
                                      (defn on-dialog-accepted ()
                                        (lang-set (cdr opt))
                                        (push-menu "title-screen" '(3)))
                                      (defn on-dialog-declined ()
                                        (show-opts)))))
                                 lang-opts)
                        (dialog (tr "Pick a language:")))))

  (when (not (equal (device-info 'name) "GameboyAdvance"))
    (setup-keypad-opts)

    (let ((lighting (lookup 'lighting (settings-load))))
      (dialog-opts-push (if (equal lighting "on")
                            (tr "Disable Lighting Effects")
                            (tr "Enable Lighting Effects"))
                        (lambda ()
                          (setq lighting (if (equal lighting "on") "off" "on"))
                          (edit-setting 'lighting lighting)
                          (dialog (format (tr "Dynamic lighting graphical effects %")
                                          (if (equal lighting "on")
                                              (tr "enabled!")
                                              (tr "disabled!"))))
                          (defn on-dialog-closed ()
                            (setq on-dialog-closed nil)
                            (show-opts))))))

  (let ((rumble (lookup 'rumble (settings-load))))
    (dialog-opts-push (if (equal rumble "on")
                          (tr "Disable Rumble")
                          (tr "Enable Rumble"))
                      (lambda ()
                        (setq rumble (if (equal rumble "on") "off" "on"))
                        (edit-setting 'rumble rumble)
                        (dialog (format (tr "Rumble %")
                                        (if (equal rumble "on")
                                            (tr "enabled!")
                                            (tr "disabled!"))))
                        (defn on-dialog-closed ()
                          (setq on-dialog-closed nil)
                          (show-opts)))))


  (dialog-opts-push (tr "Exit")
                    (lambda ()
                      (push-menu "title-screen" '(3)))))


(show-opts)
