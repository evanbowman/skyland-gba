;;;
;;; apitest.lisp
;;;
;;; This test case file tests for the correct behavior of the skyland lisp
;;; sdk. It is in a separate file to allow for running unittest.lisp with the
;;; command line build of the skyland lisp interpreter--the functions tested in
;;; this file require the skyland executable.
;;;


(gc)

(global 'put 'temp)

(setq put log)
(global 'current-test)


(defn assert-v (v)
  (when (not v)
    (error (format "in test %: assert failed! %" current-test v))))

(defn assert-eq (lhs rhs)
  (when (not (equal lhs rhs))
    (error (format "in test %: expected % not equal %"
                   current-test
                   lhs
                   rhs))))


(defn begin-test (name)
  (setq current-test name)
  (let ((msg (string "running tests: " name "...")))
    (when (bound? 'regr-print)
      (regr-print msg))
    (put msg)))

(defn end-test ()
  (put " passed!")
  (setq current-test nil)
  (gc))

(defn ensure (result)
  (when (error? result)
    (fatal result)))



(begin-test "misc")

(assert-eq 767268228 (hash '(8 . 7)))

(assert-eq "image" (read-ini "/scripts/data/cart/cart7.ini"
                             "contents"
                             "type"))

(end-test)



(begin-test "file")

;; This file that we're running must exist...
(assert-v (file-exists? "/scripts/data/unittest.lisp"))

(setq temp (file-load "/scripts/data/test-data.txt"))

(assert-eq "Here's some text..."
           (bytes-to-string (filter (lambda (c)
                                      ;; Some files, particularly the ones
                                      ;; appended to rom, have trailing null
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


(regr-print "all tests passed!")

(unbind 'begin-test
        'end-test
        'ensure
        'assert-v
        'assert-eq
        'put)

(gc)
