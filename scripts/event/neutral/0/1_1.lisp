;;;
;;; neutral/0/1_1.lisp
;;;
;;; Anvil Annie is a highly paranoid hermit. I thought about making different
;;; dialog when playing as goblins, but you know what, I think I'll keep it. Even
;;; crazy consipracy theorists are right sometimes. A broken clock is right twice
;;; a day, so they say. It also means her character works on multiple levels:
;;; Surface level: Crazy hermit with trust issues.
;;; Meta level: Commentary on how truth and madness can coexist.
;;; It lets the context completely reframe the character without changing a
;;; word. When playing as goblins, when meeting this character for the first
;;; time, one might think, she's a hardened realist. Then later when running into
;;; this character as humans -- no wait, she's entirely crazy.
;;;


(dialog
 "A small but heavily fortified island appears on your scanners. Scattered debris floats nearby - twisted metal and charred wood from recent conflicts...")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((hull 0 12) (ballista 0 11) (cannon 0 10) (hull 0 13) (masonry 0 14 3) (hull 1 10) (power-core 1 12) (masonry 1 14 3) (missile-silo 2 10) (masonry 2 14 3) (masonry 3 11 3) (masonry 3 12 3) (masonry 3 14 3) (shrubbery 3 9) (hull 3 10) (masonry 3 13 3) (hull 4 11) (canvas 4 12 (64 -536840192 973887664 941621762 1155265 -1318431487 71595039 1149989184 789571075 -281183620 1587057927 -997232256 119726179 1046381783 503348496 1750011868 6 60 16 0)) (windmill 4 14) (hull 4 10) (hull 5 11) (canvas 5 12 (42 -1593804800 -2088697312 469770270 1621229768 1210056818 389277569 254175190 1715241193 13171200 253756484 0 56)) (lemon-tree 5 9) (hull 6 11) (canvas 6 12 (28 -862298880 3184651 1108289 1712167477 -2055305215 2146439166 131 252 30 224)) (masonry 6 14 3) (bronze-hull 6 13) (hull 6 10)))

(flag-show (opponent) flag-id-old-empire)


(defn/temp attack-player ((txt . string))
  (opponent-mode 'hostile)
  (await (dialog* txt))
  (await (dialog* "Your opponent has locked weapons, prepare for battle!")))


(defn/temp pay-toll (toll)
  (if (< (coins) toll)
      (progn
        (adventure-log-add 68 '())
        (attack-player "<c:Anvil Annie:44>Broke, eh? Well, Sweet Bertha doesn't work for free! Time to collect your scrap as payment instead!"))
      (progn
        (adventure-log-add 69 (list toll))
        (coins-add (- toll))
        (dialog "<c:Anvil Annie:44>There we go! See how easy that was? Now get out of my airspace before I change my mind!")
        (exit))))


(defn/temp refuse ()
  (adventure-log-add 70 '())
  (attack-player "<c:Anvil Annie:44>I KNEW IT! Only goblin spies would refuse to pay! Time to blast some answers out of you!"))


(defn/temp strength (isle)
  (length (filter (lambda (r)
                    (equal 'weapon (rinfo 'category (car r))))
                  (rooms isle))))


(defn/temp join-crew ()
  (if (dialog-await-binary-q
       "<c:Anvil Annie:44>...That's a powerful warship you're flying. <B:0> <s:3>. . . . . <s:0> <B:0> Alright. New calculation. <B:0> If you wanted me dead, I'd already BE dead. And if you're hunting what I THINK you're hunting... <B:0> ...maybe I've been watching the wrong skies. <B:0> You gotta let me aboard, we should join up!"
       "Welcome aboard!"
       "Politely decline.")
      (let ((goblin nil)
            ((x . y) (find-crew-slot "<c:Anvil Annie:44>I'm sure we can find some space!"
                                     'ladder
                                     "Place block (1x2):")))
        (foreach (lambda (chr)
                   (if (equal (lookup 'race (cddr chr)) 1)
                       (setq goblin true)))
                 (chrs (player)))
        (chr-new (player)
                 x
                 y
                 (if goblin
                     'hostile
                     'neutral)
                 (if goblin
                     '((race . 2)) ; hostile human
                     '((icon . 44))))
        (if goblin
            (progn
              (await (dialog* "<c:Anvil Annie:44>Finally aboard! Let me just... <B:0> <s:3>. . . <s:0>"))
              (await (dialog* "<c:Anvil Annie:44>Wait. WAIT. <B:0> Those biosigns... <B:0> GOBLINS!? <B:0> I KNEW IT! This was a trap all along!"))
              (attack-player "Anvil Annie has turned hostile!"))
            (progn
              (await (dialog* "Anvil Annie joined your crew!"))
              (exit))))
      (progn
        (await (dialog* "<c:Anvil Annie:44>Understood, right. Hard to trust anyone, these days..."))
        (exit))))


(defn/temp intimidate ()
  (let ((pstr (strength (player)))
        (ostr (strength (opponent))))
    (await (dialog* "<s:3>. . . . . <s:0>"))
    (cond
      ((> pstr ostr)
       (join-crew)) ; she joins you
      ((or (> pstr 2)
           (and (> pstr 1)
                (chance 2)))
       (if (dialog-await-binary-q "<c:Anvil Annie:44>Your fortifications... That's not goblin salvage work. Human military refit. Maybe even pre-migration hardware. <B:0> 400@. Black ops rates. <B:0> And you sail straight through - no loitering, no surveys, no 'equipment malfunctions' near my perimeter!"
                                  "Pay 400@."
                                  "Refuse bribe.")
           (pay-toll 400)
           (refuse)))
      (true
       (if (dialog-await-binary-q "<c:Anvil Annie:44>...HA! Oh, that's PRECIOUS! <B:0> You actually thought - with THAT floating scrapyard - you could intimidate ME? <B:0> 800@ now. And count yourself lucky I don't raise it any higher!"
                                  "Pay 800@."
                                  "Refuse bribe.")
           (pay-toll 800)
           (refuse))))))


(defn/temp negotiate ()
  (let ((sel (await (dialog-choice* "<c:Anvil Annie:44>Alright, let's negotiate. <B:0> I'm listening, but no sudden moves, gotit?"
                                    (list "Pay 700@."
                                          (let ((st (strength (player))))
                                            (cond
                                              ((> st 2) "Intimidate.")
                                              ((equal st 2) "Intimidate. (risky)")
                                              (true "Intimidate. (foolish)")))
                                          "Refuse Bribe.")))))
    (case sel
      (0 (pay-toll 700))
      (1 (intimidate))
      (2 (refuse)))))


(defn on-converge ()
  (setq on-converge nil)
  (await (dialog* "<c:Anvil Annie:44>Unknown vessel! You're trespassing in MY airspace! <B:0> "
                  "I've been keeping this stretch of sky clear for twenty years, and I don't plan on stopping now! <B:0> "
                  "Don't even think about giving me that 'innocent trader' routine - I can smell goblin sympathizers from three leagues away!"))
  (dialog-opts-reset)
  (if (dialog-await-binary-q-w/lore
       "<c:Anvil Annie:44>Tell you what, spy - <B:0>you hand over 700@ right now, and maybe Sweet Bertha here won't blow a hole in your hull! (pats cannon)"
       "Negotiate."
       "Refuse Bribe."
       '(("We aren't spies!" .
          "<c:Anvil Annie:44>Course you'd say that. Real spies always deny it. Innocent traders? They get confused, ask what I'm talking about. But you knew EXACTLY what I meant...")
         ("What's this debris field?" .
          "<c:Anvil Annie:44>Spies, saboteurs, sleeper agents! They think they're so clever with their fake distress calls and forged cargo manifests! But I can spot 'em from leagues away! <B:0> See that scorched hull fragment? 'Friendly trader' who tried to scan my weapon configurations! <B:0> The nerve! They're ALL connected, part of some massive intelligence network! <B:0> Anyway...")))
      (negotiate)
      (refuse)))
