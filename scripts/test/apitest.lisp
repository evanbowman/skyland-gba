;;;
;;; apitest.lisp
;;;
;;; This test case file tests for the correct behavior of the SKYLAND LISP
;;; SDK. It is in a separate file to allow for running unittest.lisp with the
;;; command line build of the SKYLAND LISP interpreter -- the functions tested
;;; in this file require the Skyland executable.
;;;


(gc)

(global 'put 'temp)

(setq put log)
(global 'current-test)


(defn assert-v (v)
  (when (not v)
    (error (format "In test %: assert failed! %" current-test v))))

(defn assert-eq (lhs rhs)
  (when (not (equal lhs rhs))
    (error (format "In test %: expected % not equal %"
                   current-test
                   lhs
                   rhs))))


(defn begin-test (name)
  (setq current-test name)
  (let ((msg (string "Running tests: " name "...")))
    (when (bound? 'regr-print)
      (regr-print msg 1 3))
    (put msg)))

(defn end-test ()
  (put " passed!")
  (setq current-test nil))

(defn ensure (result)
  (when (error? result)
    (fatal result)))



(begin-test "pools")

(global 'pools)
(setq pools (mem-pools))
(assert-v (not (nil? (find 'entity-mem pools))))
(assert-v (not (nil? (find 'room-mem pools))))
(assert-v (not (nil? (find 'receive-packet-pool pools))))
(assert-v (not (nil? (find 'transmit-packet-pool pools))))
(assert-v (not (nil? (find 'dynamic-texture-pool pools))))
(assert-v (not (nil? (find 'entity-list-node pools))))
(assert-v (not (nil? (find 'scenes pools))))
(assert-v (not (nil? (find 'scratch-buffers pools))))

(setq temp (mem-pool-info 0))
(assert-v (assoc 'size (cdr temp)))
(assert-v (assoc 'used (cdr temp)))
(assert-v (assoc 'count (cdr temp)))


(defn pool-usage (pool-sym)
  (let ((sym pool-sym))
    (let ((matched (filter (lambda (p)
                             (equal sym (car (mem-pool-info p))))
                           (range 0 (length pools)))))
      (apply + (map (lambda (pl)
                      (lookup 'used (cdr (mem-pool-info pl))))
                    matched)))))

(island-configure (player) '())
(opponent-init 3 'neutral)
(island-configure (opponent) '())

(assert-eq 0 (pool-usage 'room-mem))
(island-configure (player) '((power-core 1 13)))
(assert-eq 1 (pool-usage 'room-mem))
(room-del (player) 1 13)
(assert-eq 0 (pool-usage 'room-mem))

(assert-eq 0 (pool-usage 'entity-mem))
(island-configure (player) '((power-core 1 13)))
(chr-new (player) 1 14 'neutral nil)
(assert-eq 1 (pool-usage 'entity-mem))
(room-del (player) 1 13)
(assert-eq 0 (pool-usage 'entity-mem))

(unbind 'pool-usage)

(end-test)



(begin-test "isle")

(opponent-init 4 'neutral)

(assert-v (wrapped? (player)))
(assert-v (wrapped? (opponent)))

(assert-v (not (equal (player) (opponent))))
(assert-eq (player) (player))

(assert-eq (type (player)) 'isle)

(assert-eq (userdata-tag (unwrap (player))) 1)
(assert-eq (userdata-tag (unwrap (opponent))) 2)

(assert-eq "#(isle:player)" (string (player)))
(assert-eq "#(isle:opponent)" (string (opponent)))

(terrain-set (player) 4)

(island-configure
 (player)
 '((power-core 1 13)
   (cannon 3 13)
   (cannon 3 14)))

(assert-eq 3 (length (rooms (player))))
(groups-add 'Up 3 13)
(assert-eq '((3 . 13)) (lookup 'Up (groups)))
(groups-reset)
(assert-eq nil (lookup 'Up (groups)))

(assert-eq 2 (room-count (player) 'cannon))
(room-new (player) '(cannon 3 12))
(assert-eq 3 (room-count (player) 'cannon))
(assert-eq '(power-core 1 13) (room-load (player) 1 13))
(assert-v (room-is-critical (player) 1 13))
(room-new (player) '(power-core 1 11))
(assert-v (not (room-is-critical (player) 1 13)))

(assert-eq (chr-slots (player)) '((2 . 14) (2 . 12) (1 . 14) (1 . 12)))
(assert-eq (construction-sites (player) '(1 . 2)) '((0 . 13) (1 . 9) (2 . 9) (3 . 10)))
(assert-eq (construction-sites (player) '(2 . 2)) '((1 . 9) (2 . 9)))
(assert-eq (construction-sites (player) '(3 . 2)) '((1 . 9)))

(room-new (player) '(cargo-bay 0 13))
(cargo-set (player) 0 13 "cargo-test")
(assert-eq "cargo-test" (cargo (player) 0 13))

(assert-eq 4 (terrain (player)))

(opponent-init 7 'hostile)
(assert-eq (terrain (opponent)) 7)

(room-mut (player) 0 13 'missile-silo)
(assert-eq '(missile-silo 0 13) (room-load (player) 0 13))

(assert-eq 240 (lookup 'max-hp (room-meta 'hull)))

(end-test)


(begin-test "world graph")

(wg-generate)
(assert-eq
 (wg-nodes)
 '((1 0 . 5) (3 4 . 7) (2 8 . 5) (3 12 . 5) (3 16 . 4) (5 20 . 7) (9 7 . 11) (8 17 . 7) (9 19 . 10) (9 16 . 11) (3 20 . 1) (3 11 . 11) (9 7 . 8) (6 3 . 11) (9 17 . 1) (9 10 . 2) (8 11 . 8) (3 14 . 8)))

(assert-eq (wg-path '(0 . 5) '(8 . 5))
           '((0 . 5) (4 . 7) (8 . 5)))

(end-test)


(begin-test "chr")

(island-configure
 (player)
 '((power-core 1 13)))

(setq temp (chr-new (player) 1 14 'neutral '((icon . 1))))
(assert-v (not (equal temp (chr-new (player) 1 14 'neutral nil))))
(assert-eq (chrs (player)) '((1 14 (icon . 1) (id . 2)) (1 14 (id . 3))))
(assert-eq (load-commentary "regression_test") "Working!")
(chr-del (player) 1 14)
(chr-del (player) 1 14)
(assert-eq (load-commentary "regression_test") nil)

(end-test)


(begin-test "misc")

(assert-eq 767268228 (hash '(8 . 7)))

;; Just make sure that reading a non-existent ini key returns nil...
(assert-eq nil (read-ini "/scripts/data/character_inter.ini"
                         "character_1"
                         "welcomes_9000"))

(end-test)



(begin-test "cart")

(assert-eq "image" (read-ini "/scripts/data/cart/cart7.ini"
                             "contents"
                             "type"))

(assert-eq "SKYLAND" (car (cart-info 0)))

(end-test)



(begin-test "hooks")

(assert-v (and (bound? 'on-fadein)
               (bound? 'on-converge)
               (bound? 'on-dialog-closed)
               (bound? 'on-room-destroyed)
               (bound? 'on-victory)
               (bound? 'on-crew-died)
               (bound? 'on-shop-item-sel)
               (bound? 'on-shop-enter)))

(end-test)



(begin-test "misc")

(coins-set 55)
(assert-eq 55 (coins))
(coins-add 40)
(assert-eq (coins) 95)

;; This has to be true to get into the regression module.
(assert-v (is-developer-mode))

(assert-eq 15 ((eval-file "/scripts/data/test-eval.lisp") 5))

(end-test)



(begin-test "file")

;; This file that we're running must exist...
(assert-v (file-exists? "/scripts/test/unittest.lisp"))

(setq temp (file-load "/scripts/data/test-data.txt"))

(assert-eq "Here's some text..."
           (bytes-to-string (filter (lambda (c)
                                      ;; Some files, particularly the ones
                                      ;; appended to ROM, have trailing null
                                      ;; bytes for padding purposes. Also,
                                      ;; filter out the ending newline.
                                      (and (> c 0) (not (equal c 10))))
                                    (file-read temp 0 (file-size temp)))))


(let ((f (file-load "/scripts/stdlib.lisp")))
  (assert-v (and (error? f)
                 (equal (error-info f)
                        "file too large to load!"))))


(let ((file (file-load "/test.dat"))
      (str "some text!"))

  (ensure (assert-eq "#(file:/test.dat)" (string file)))

  (file-write! file 0 (string-to-bytes str))
  (file-store file)

  (setq file (file-load "/test.dat"))
  (ensure (assert-eq str (bytes-to-string (file-read file 0 (length str)))))

  ;; Append the string again, to the end of the file. Later, we'll read it back
  ;; and make sure that there are two copies of the string back-to-back.
  (file-write! file -1 (string-to-bytes str))
  (file-store file)

  (setq file (file-load "/test.dat"))
  (ensure (assert-eq (string str str) (bytes-to-string (file-read file 0 (* 2 (length str))))))
  (assert-eq (file-size file) (* 2 (length str))))


(assert-v (file-exists? "/test.dat"))
(file-unlink "/test.dat")
(assert-v (not (file-exists? "/test.dat")))

(end-test)


(regr-print "All tests passed!" 1 3)

(unbind 'begin-test
        'end-test
        'ensure
        'put)

(regr-print "Linting all scripts!" 1 3)

(defn ends-with (str sufx)
  (let ((m1 (string-explode str))
        (m2 (string-explode sufx)))
    (equal (slice m1 (- (length m1) (length m2))) m2)))

(setq temp 0)

;; This includes variable declarations that are not processed at startup. The
;; linter needs them!
(eval-file "/scripts/adventure_vars.lisp")

(let ((last nil))
  (filesystem-walk
   "/"
   (lambda (path)
     (gc)
     (when (ends-with path ".lisp")
       (+= temp 1)
       (let ((indent 1)
             (y 5)
             (spl (split path "/")))
         (if (< (length spl) (length last))
             ;; Clean up lines if the directory depth is less than last printed.
             (foreach (lambda (yy)
                        (regr-print "" 1 (+ y (- yy 1))))
                      (range (length spl) (length last))))
         (setq last spl)
         (foreach (lambda (dir)
                    (regr-print dir indent y)
                    (+= indent 2)
                    (+= y 1))
                  (cdr spl))
         (let ((r (lint-file path)))
           (if (error? r)
               (fatal (string "in file " (cdr spl) ": " r)))))))))


;; Clear directory listing from screen.
(foreach (lambda (y)
           (regr-print "" 1 y))
         (range 5 10))


(assert-eq 6 (lisp-mem-stack-used))

(unbind 'ends-with
        'assert-v
        'assert-eq)
(gc)
