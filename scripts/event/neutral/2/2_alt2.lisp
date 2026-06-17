;;;
;;; neutral/2/2_alt2.lisp
;;;

(tr-bind-current)

(dialog (tr "A ship runs lit and steady against the dark, every system answering. <B:0> That is the first wrong thing. <B:0> The same figure stands at every window. One face, copied the length of the hull. <B:0> The nearest move and work. The farthest do not move at all. <B:0> Nothing hails you. The ship notes your approach, and a channel opens."))


(opponent-init 13 'neutral)


(procgen)
(opponent-mode 'neutral)


(flag-show (opponent) flag-id-old-empire)


(defn/temp find-chr (x y)
  (let ((opts (chrs (player)))
        (xy (cons x y)))
    (if-let ((match (filter (lambda (info)
                              (equal xy (cons (get info 0)
                                              (get info 1))))
                            opts)))
        (car match)
      nil)))


(global 'donate-crew) ;; For the linter


(defn/temp present-choice ()
  (case (dialog-await-choice
         (tr "The channel stays open. At the windows, the copies have stopped working — every one turned toward you.")
         (tr '("We can spare someone."
               "Put it to rest."
               "We can't help you.")))
    (0 (if (chrs (player))
           (donate-crew)
           (progn
             (dialog-await (tr "You have no one left to give."))
             (present-choice))))
    (1 (dialog-await (tr (s+ "You bring your weapons to bear. The ship answers. <B:0> "
                             "The nearest figures work the guns with the same blank diligence "
                             "they showed at every other task — no anger in it. <B:0> "
                             "The far ones do not move at all.")))
       (opponent-mode 'hostile)
       (foreach (lambda (info)
                  (let ((x (get info 0))
                        (y (get info 1)))
                    (chr-del (opponent) x y)
                    (let ((id (chr-new (opponent) x y 'hostile '((race . 2)))))
                      (chr-hp id 48))))
                (chrs (opponent)))
       (dialog-await (tr "<c:Carrier:25>DEFENSE PROTOCOL ENGAGED. ASSETS COMMITTED.")))
    (2 (exit))))


(defn/temp get-item ()
  (let ((tab (eval-file "/scripts/config/room_tab.lisp"))
        (opt '()))

    (while (nil? opt)
      (let ((pick (sample tab)))
        (let ((cost (get pick 2)))
          (when (> cost 2999)
            (setq opt (cons (car pick) opt))))))

    (let (((x . y) (await (sel-input* opt (tr "Place item:")))))   ;; paren fixed
      (room-new (player) (list opt x y))
      (sound "build0")
      (dialog-await (tr "<c:Carrier:25>EXCHANGE COMPLETE. THE LEDGER IS BALANCED.")))))


(defn/temp on-donate (x y properties)
  (let ((icon (lookup 'icon properties)))
    (chr-del (player) x y)
    (dialog-await (tr "Across the gap, your crewmate takes a place at the glass, and does not look back."))
    (sleep 200)
    (dialog-await (tr "<c:Carrier:25>TEMPLATE ACCEPTED. PRODUCING FIRST COPY..."))
    (sleep 1000)
    (dialog-await (tr "Something arrives in the bay — your crewmate's face, new and unworn."))
    (let ((id (chr-new (player) x y 'neutral (cons '(rplc . 1) properties))))
      (chr-hp id (* 255 3/4)))
    (dialog-await (string (if icon
                              (format "<c:%:%> " (tr "Replicant") icon)
                              (format "<c:%:19> " (tr "Replicant")))
                          (tr "Something feels off. <B:0> Hm. Probably nothing.")))
    (get-item)
    (dialog-await (tr "The replicant vessel drifts smoothly into the distance..."))
    (exit)))


(defn/temp donate-crew ()
  (let* (((x . y) (await (sel-input* nil (tr "Pick a crewmember:"))))
         (chr (find-chr x y)))
    (if chr
        (let ((icon (lookup 'icon (cddr chr))))
          (case (dialog-await-choice (tr (string (if icon
                                                     (format "<c:%:%> "
                                                             (tr "Crewmember")
                                                             icon)
                                                     "")
                                                 (tr "Are you sure? I will go if needed...")))
                                     (tr '("Accept" "Pick someone else" "Nevermind")))
            (0 (on-donate x y (cddr chr)))
            (1 (donate-crew))
            (2 (present-choice))))
        (donate-crew))))


(defn on-converge ()
  (setq on-converge nil)
  (dialog-await (tr "<c:Carrier:25>CONTACT LOGGED. VESSEL VIABLE. <B:0> CREW INTEGRITY: 31%. GENERATION 1,408. THE LINE NO LONGER HOLDS FORM. <B:0> DESIGNATE ONE CREW MEMBER AS TEMPLATE. THE LINE WILL RESET TO GENERATION ZERO. <B:0> THE TEMPLATE REMAINS WITH THIS VESSEL. <B:0> A FIRST COPY IS ISSUED TO YOU, ALONG WITH A VALUABLE ITEM. <B:0> THE EXCHANGE IS EQUAL."))

  (present-choice))
