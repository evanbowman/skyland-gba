;;;
;;; neutral/0/1_1.lisp
;;;

;; Anvil Annie is a highly paranoid hermit. I thought about making different
;; dialog when playing as goblins, but you know what, I think I'll keep it. Even
;; crazy consipracy theorists are right sometimes. A broken clock is right twice
;; a day, so they say. It also means her character works on multiple levels:
;; Surface level: Crazy hermit with trust issues
;; Meta level: Commentary on how truth and madness can coexist
;; It lets the context completely reframe the character without changing a
;; word. When playing as goblins, when meeting this character for the first
;; time, one might think, she's a hardened realist. Then later when running into
;; this character as humans--no wait, she's entirely crazy.

(dialog
 "A small but heavily fortified island appears on your scanners. Scattered debris floats nearby - twisted metal and charred wood from recent conflicts...")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((hull 0 12) (ballista 0 11) (cannon 0 10) (hull 0 13) (masonry 0 14 3) (hull 1 10) (power-core 1 12) (masonry 1 14 3) (missile-silo 2 10) (masonry 2 14 3) (masonry 3 11 3) (masonry 3 12 3) (masonry 3 14 3) (shrubbery 3 9) (hull 3 10) (masonry 3 13 3) (hull 4 11) (canvas 4 12 (64 -536840192 973887664 941621762 1155265 -1318431487 71595039 1149989184 789571075 -281183620 1587057927 -997232256 119726179 1046381783 503348496 1750011868 6 60 16 0)) (windmill 4 14) (hull 4 10) (hull 5 11) (canvas 5 12 (42 -1593804800 -2088697312 469770270 1621229768 1210056818 389277569 254175190 1715241193 13171200 253756484 0 56)) (lemon-tree 5 9) (hull 6 11) (canvas 6 12 (28 -862298880 3184651 1108289 1712167477 -2055305215 2146439166 131 252 30 224)) (masonry 6 14 3) (bronze-hull 6 13) (hull 6 10)))

(flag-show (opponent) 4)


(defn on-converge ()
  (dialog
   "<c:anvil annie:44> Unknown vessel! You're trespassing in MY airspace! <B:0> I've been keeping this stretch of sky clear for twenty years, and I don't plan on stopping now! <B:0> Don't even think about giving me that 'innocent trader' routine - I can smell goblin sympathizers from three leagues away! <B:0> Tell you what, spy - <B:0>you hand over 700@ right now, and maybe Sweet Bertha here won't blow a hole in your hull! (pats cannon)")
  (dialog-opts-reset)
  (dialog-await-binary-q-w/lore "pay 700@" "refuse bribe"
                                '(("we aren't spies!" .
                                   "<c:anvil annie:44> Course you'd say that. Real spies always deny it. Innocent traders? They get confused, ask what I'm talking about. But you knew EXACTLY what I meant...")
                                  ("what's this debris field?" .
                                   "<c:anvil annie:44> Spies, saboteurs, sleeper agents! They think they're so clever with their fake distress calls and forged cargo manifests! But I can spot 'em from leagues away! <B:0> See that scorched hull fragment? 'Friendly trader' who tried to scan my weapon configurations! <B:0> The nerve! They're ALL connected, part of some massive intelligence network! <B:0> Anyway...")))
  (setq on-converge nil))


(let ((scr
       (lambda (txt)
         (dialog txt)
         (opponent-mode 'hostile))))
  (setq on-dialog-accepted
        (lambda ()
          (if (< (coins) 700)
              (progn
                (adventure-log-add 68 '())
                (scr "<c:anvil annie:44> Broke, eh? Well Sweet Bertha doesn't work for free! Time to collect your scrap as payment instead!"))
            (progn
              (adventure-log-add 69 (list 700))
              (coins-add -700)
              (dialog "<c:anvil annie:44> There we go! See how easy that was? Now get out of my airspace before I change my mind!")
              (exit)))))


  (setq on-dialog-declined
        (lambda ()
          (adventure-log-add 70 '())
          (scr "<c:anvil annie:44>I KNEW IT! Only goblin spies would refuse to pay! Time to blast some answers out of you!"))))
