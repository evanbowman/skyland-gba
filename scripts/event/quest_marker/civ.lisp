;;;
;;; quest_marker/civ.lisp
;;;


(dialog "You arrive at the location that the orphan boy marked on your map. <B:0> "
        "<b:/scripts/data/img/ornate.img.bin>"
        "As you approach, an advanced walled city emerges from the clouds. Its gleaming canals and skillful stonework shimmer with a brilliant light...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((forcefield* 0 9)
   (forcefield 0 11)
   (bronze-hull 0 14)
   (forcefield 0 12)
   (forcefield 0 10)
   (forcefield* 0 13)
   (forcefield 1 7)
   (water-source 1 14)
   (forcefield* 1 8)
   (water-source 1 13)
   (bronze-hull 2 11)
   (masonry 2 14 0)
   (masonry 2 12 0)
   (shrubbery 2 10)
   (forcefield 2 7)
   (masonry 2 13 0)
   (war-engine 3 9)
   (forcefield* 3 7)
   (masonry 3 13 3)
   (water-source 3 14)
   (masonry 4 13 3)
   (masonry 4 14 0)
   (forcefield* 4 7)
   (forcefield* 5 7)
   (water-source 5 14)
   (masonry 5 13 3)
   (masonry 6 14 0)
   (forcefield 6 7)
   (bronze-hull 6 11)
   (masonry 6 12 0)
   (masonry 6 13 0)
   (lemon-tree 6 9)
   (forcefield 7 7)
   (water-source 7 13)
   (forcefield* 7 8)
   (water-source 7 14)
   (forcefield 8 10)
   (forcefield 8 11)
   (forcefield 8 12)
   (forcefield* 8 13)
   (bronze-hull 8 14)
   (forcefield* 8 9)))

(secret 4 14 "Because they're highly reclusive and technologically advanced, many myths exist about the Sylph. But they're only human...")


(let ((id (lookup 6 qvar))
      (boy nil))

  (map (lambda (chr)
         (if (equal id (lookup 'id (cddr chr)))
             (setq boy true)))
       (chrs (player)))

  (if boy
      (defn on-converge ()
        (dialog "<c:Sylph:21><S:1>Hello, traveller...")

        (defn on-dialog-closed ()
          (dialog "<c:Orphan Boy:26><S:1>Oh!!! I'm home at last!")

          (defn on-dialog-closed ()
            (dialog "<c:Sylph:21><S:1>Oh! What have we here?!")

            (defn on-dialog-closed ()
              (map (lambda (chr)
                     (if (equal id (lookup 'id (cddr chr)))
                         (chr-del (player) (car chr) (cadr chr))))
                   (chrs (player)))
              (coins-add 2000)
              (adventure-log-add 55 nil)
              (dialog "The orphan boy returned to his home!")
              (defn on-dialog-closed ()
                (dialog "<c:Sylph:21>Hello, traveller...<B:0> I am very grateful to you for bringing him back! ...")
                (setq on-dialog-closed (lambda ()
                                         (on-timeout 500 'fut)
                                         (setq on-dialog-closed nil)))
                (defn fut ()
                  (sound "bell")
                  (sound "thunder_close_1")
                  (effect "lightning" 0 0)
                  (opponent-reset)
                  (wg-storm-frontier-set (max (list (- (wg-storm-frontier) 3) 1)))

                  (on-timeout 1000 'fut)

                  (defn fut ()
                    (dialog "A flash of resplendent light emanates from the city... <B:0> the approaching storm clouds recede far into the horizon... <B:0> The Sylph castle seems to have also transported some strange block onto your island... <B:0> Where do you want to place it?")
                    (unbind 'fut)
                    (defn on-dialog-closed ()
                      (run-util-script "place-new-block"
                                       'amplifier
                                       "Place amplifier:"
                                       (lambda (x y)
                                         (exit)))))))))))

    (defn on-converge ()
      (dialog "Despite multiple attempts to contact the city, the inhabitants are unresponsive. It's too bad the child isn't aboard your island anymore, maybe he'd know what this was all about...")
      (exit))))
