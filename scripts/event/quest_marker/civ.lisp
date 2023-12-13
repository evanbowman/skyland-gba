
(dialog "You arrive at the location that the orphan boy marked on your map. <B:0> "
        "<b:/scripts/misc/img/ornate.skg>"
        " As you approach, an ornate walled city emerges from the clouds. Its gleaming canals and skillful stonework shimmer with an otherworldly light...")


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


(let ((id (lookup 6 qvar))
      (boy nil))

  (map (lambda
         (if (equal id (lookup 'id (cddr $0)))
             (setq boy true)))
       (chrs (player)))

  (if boy
      (defn on-converge [0]
        (dialog "<c:sky sprite:21> Hello, traveller... <B:0> It's been quite a long while since we've seen humans. You are very lost indeed...")

        (defn on-dialog-closed [0]
          (dialog "<c:orphan boy:26> " (rot13 "Oh!!! I'm home at last!"))

          (defn on-dialog-closed [0]
            (dialog "<c:sky sprite:21>Uryyb! Why, who's this!? He's one of us, you know. <B:0> We're very grateful to you for binging him to us. Normally we try not to intervene... but just this once, we'll help you out…")

            (defn on-dialog-closed [0]
              (map (lambda
                     (if (equal id (lookup 'id (cddr $0)))
                         (chr-del (player) (car $0) (cadr $0))))
                   (chrs (player)))
              (coins-add 3000)
              (adventure-log-add 55 nil)
              (sound "bell")
              (wg-storm-frontier-set (max (list (- (wg-storm-frontier) 3) 1)))
              (dialog "A flash of resplendant light emanates from the city... <B:0> the approaching storm clouds receed far into the horizon...")
              (setq on-dialog-closed exit)))))

    (defn on-converge [0]
      (dialog "Despite multiple attempts to contact the city, the inhabitants are unresponsive. It's too bad the child isn't aboard your island anymore, maybe he'd know what this was all about...")
      (exit))))
