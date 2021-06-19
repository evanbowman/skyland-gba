

(init-opponent 5 'neutral)


(dialog
 "A fortress appears in the distance, slowly rising above the mist."
 "The residents appear to be friendly...")


(set 'after-fadein
     (lambda
       (dialog "% would like to join your crew, enlist? %$")
       (set 'after-fadein nil)))


(set 'after-approach
     (lambda
       (dialog "% would like to join your crew, enlist? %$")
       (set 'after-approach nil)))
