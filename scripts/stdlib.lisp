;;;
;;; stdlib.lisp
;;;
;;; A small standard library
;;;


(macro or (EXPR)
 `(if ,(car EXPR)
      1
    ,(if (cdr EXPR)
         (cons 'or (cdr EXPR))
       0)))


(macro and (EXPR)
 `(if (not ,(car EXPR))
      nil
    ,(if (cdr EXPR)
         (cons 'and (cdr EXPR))
       1)))

(macro cond (EXPR)
 `(if ,(car (car EXPR))
      ,(cons 'progn (cdr (car EXPR)))
    ,(if (cdr EXPR)
         (cons 'cond (cdr EXPR))
       nil)))


(macro -case-r (EXPR)
 `(if (equal --TEMP-CASE-V ,(car (car EXPR)))
      ,(cons 'progn (cdr (car EXPR)))
    ,(if (cdr EXPR)
         (if (equal (car (car (cdr EXPR))) 'else)
             (cons 'progn (cdr (car (cdr EXPR))))
           (cons '-case-r (cdr EXPR)))
       nil)))

(macro case (EXPR)
       `(let ((--TEMP-CASE-V ,(car EXPR)))
          ,(cons '-case-r (cdr EXPR))))


(macro let* (BINDINGS BODY)
  (if (nil? (cdr BINDINGS))
    `(let (,(car BINDINGS)) ,@BODY)
    `(let (,(car BINDINGS))
       (let* ,(cdr BINDINGS) ,@BODY))))


(macro when-let (BINDING BODY)
  `(let ,BINDING
     (when ,(caar BINDING)
       ,@BODY)))


(macro if-let (BINDING IF-BR ELSE-BR)
  `(let ,BINDING
     (if ,(caar BINDING)
         ,IF-BR
         ,@ELSE-BR)))


;; Currently unused...
;; (macro ->> (VAL FORMS)
;;   (if (nil? (cdr FORMS))
;;     (append (car FORMS) (list VAL))
;;     `(->> ,(append (car FORMS)
;;                    (list VAL))
;;           ,@(cdr FORMS))))


;; Some useful macros for defining functions

;; The new defn macros, to replace the old ones using positional args.
(macro defn (NAME REST) `(setfn ,(cons $q NAME) (lambda ,@REST)))

;; Defines a bytecode-compiled function.  You should only compile long-lived
;; functions, because bytecode cannot be deallocated. At the same time, bytecode
;; takes up less space than non-compiled functions.
(macro defn/c (NAME REST) `(setfn ,(cons $q NAME) (compile (lambda ,@REST))))

;; Define a temporary function, to be cleaned up at the end of a level. Use for
;; script-local functions that don't need to be referenced by other scripts.
(macro defn/temp (NAME REST) `(set-temp ,(cons $q NAME) (lambda ,@REST)))


(macro += (NAME VAL)
 `(setq ,NAME (+ ,NAME ,@VAL)))

(macro push (NAME EXPR)
 `(setq ,NAME (cons ,@EXPR ,NAME)))

(macro setq (NAME EXPR) `(set ,(cons $q NAME) ,@EXPR))
(macro setq/temp (NAME EXPR) `(set-temp ,(cons $q NAME) ,@EXPR))

(macro when (EXPR BODY) `(if ,EXPR (progn ,@BODY)))
(macro unless (EXPR BODY) `(if (not ,EXPR) (progn ,@BODY)))

;; Multiline string macro
(macro s+ (EXPR REST) (apply string (cons EXPR REST)))

(global 'setfn)
(setq setfn
      (compile
       (lambda ((sym . symbol) fn)
         (global sym)
         (set sym fn))))

(global 'temp-vals)

(defn append (lat1 lat2)
  ;; Not the most efficient way to implement append, but this implementation
  ;; with unquote-splicing is quite compact.
  `(,@lat1 ,@lat2))

(global 'floor)
(setq floor int) ; cast to int rounds down

;; Used by the runtime to put something on the callstack when inlining foreach.
(defn --inline-foreach ())

;; While suboptimal, these functions have the benefit of being small.
(defn/c min (lat) (car (sort lat <)))
(defn/c max (lat) (car (sort lat >)))


(defn build-library (path library-path linkage)
  (let ((store-fns nil))
    (let ((setfn (lambda (sym fn)
                   (global sym)
                   (set sym fn)
                   (setq store-fns (cons sym store-fns)))))
      (eval-file path)
      (apply (curry save-library library-path linkage)
             store-fns)
      (setq setfn nil))))


(defn load-library-cached (path library-path)
  (when (not (load-library library-path))
    (log (format "building %..." library-path))
    (build-library path library-path 'absolute)))


;; NOTE: most functions in stdlib.lisp were moved to the precompiled core
;; package, which is now loaded by init.lisp. See /packages/source/ for
;; the function implementations.
