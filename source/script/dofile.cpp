////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "lisp.hpp"
#include "platform/platform.hpp"


#include <fstream>
#include <iostream>
#include <sstream>


class Printer : public lisp::Printer
{
public:
    void put_str(const char* str) override
    {
        std::cout << str;
    }
};


const char* utilities =
    ";;;\n"
    ";;; stdlib.lisp\n"
    ";;;\n"
    ";;; A small standard library\n"
    ";;;\n"
    "\n"
    "\n"
    "(macro or (EXPR)\n"
    " `(if ,(car EXPR)\n"
    "      1\n"
    "    ,(if (cdr EXPR)\n"
    "         (cons 'or (cdr EXPR))\n"
    "       0)))\n"
    "\n"
    "\n"
    "(macro and (EXPR)\n"
    " `(if (not ,(car EXPR))\n"
    "      0\n"
    "    ,(if (cdr EXPR)\n"
    "         (cons 'and (cdr EXPR))\n"
    "       1)))\n"
    "\n"
    "\n"
    "(macro cond (EXPR)\n"
    " `(if ,(car (car EXPR))\n"
    "      ,(cons 'progn (cdr (car EXPR)))\n"
    "    ,(if (cdr EXPR)\n"
    "         (cons 'cond (cdr EXPR))\n"
    "       nil)))\n"
    "\n"
    "\n"
    "(macro dotimes (N BODY)\n"
    " `(map (fn ,@BODY) (range 0 ,N)))\n"
    "\n"
    "\n"
    ";; Some useful macros for defining functions\n"
    "\n"
    ";; The new defn macros, to replace the old ones using positional args.\n"
    "(macro defn (NAME REST) `(setfn ,(cons $q NAME) (lambda ,@REST)))\n"
    "\n"
    ";; Defines a bytecode-compiled function.  You should only compile "
    "long-lived\n"
    ";; functions, because bytecode cannot be deallocated. At the same time, "
    "bytecode\n"
    ";; takes up less space than non-compiled functions.\n"
    "(macro defn/c (NAME REST) `(setfn ,(cons $q NAME) (compile (lambda "
    ",@REST))))\n"
    "\n"
    "\n"
    "(macro += (NAME VAL)\n"
    " `(setq ,NAME (+ ,NAME ,@VAL)))\n"
    "\n"
    "(macro setq (NAME EXPR) `(set ,(cons $q NAME) ,@EXPR))\n"
    "\n"
    "(macro when (EXPR BODY) `(if ,EXPR (progn ,@BODY)))\n"
    "(macro unless (EXPR BODY) `(if (not ,EXPR) (progn ,@BODY)))\n"
    "\n"
    "(global 'setfn)\n"
    "(setq setfn\n"
    "      (compile\n"
    "       (lambda (sym fn)\n"
    "         (global sym)\n"
    "         (set sym fn))))\n"
    "\n"
    "\n"
    "(macro progn (BODY)\n"
    " `(let () ,@BODY))\n"
    "\n"
    "\n"
    "(defn/c acons (key val alst)\n"
    "  (cons (cons key val) alst))\n"
    "\n"
    "(defn/c remove-if (lat pred)\n"
    "  (let ((p pred))\n"
    "    (filter (lambda (e)\n"
    "              (not (p e)))\n"
    "            lat)))\n"
    "\n"
    "(defn/c remove (lat elem)\n"
    "  (remove-if lat (equalto? elem)))\n"
    "\n"
    "(defn/c assoc (k alst)\n"
    "  (let ((temp k))\n"
    "    (get (filter (lambda (v)\n"
    "                   (equal (car v) temp))\n"
    "                 alst)\n"
    "         0)))\n"
    "\n"
    "(defn/c lookup (key alst)\n"
    "  (let ((kvp (assoc key alst)))\n"
    "    (if kvp (cdr kvp))))\n"
    "\n"
    "(defn/c insert (elem lat pos)\n"
    "  (append (slice lat 0 pos) (cons elem (slice lat pos))))\n"
    "\n"
    "(defn append (lat1 lat2)\n"
    "  ;; Not the most efficient way to implement append, but this "
    "implementation\n"
    "  ;; with unquote-splicing is quite compact.\n"
    "  `(,@lat1 ,@lat2))\n"
    "\n"
    "(defn/c gen (func n)\n"
    "  (map func (range n)))\n"
    "\n"
    "(defn/c push (sym val)\n"
    "  (set sym (cons val (eval sym))))\n"
    "\n"
    "\n"
    "(defn/c push-set (sym val)\n"
    "  (let ((tmp (cons val (eval sym))))\n"
    "    (set sym (union tmp tmp))))\n"
    "\n"
    "\n"
    "(defn/c merge (l1 l2 comp)\n"
    "  (cond\n"
    "   ((not l1) l2)\n"
    "   ((not l2) l1)\n"
    "   ((comp (car l1) (car l2))\n"
    "    (cons (car l1) ((this) (cdr l1) l2 comp)))\n"
    "   (true (cons (car l2) ((this) l1 (cdr l2) comp)))))\n"
    "\n"
    "\n"
    "(defn/c sort (lat comp)\n"
    "  (if (not (cdr lat))\n"
    "      lat\n"
    "    (let ((len (length lat)))\n"
    "      (merge ((this) (slice lat 0 (/ len 2)) comp)\n"
    "             ((this) (slice lat (/ len 2)) comp)\n"
    "             comp))))\n"
    "\n"
    ";; While suboptimal, these functions have the benefit of being small.\n"
    "(defn/c min (lat) (car (sort lat <)))\n"
    "(defn/c max (lat) (car (sort lat >)))\n"
    "\n"
    "(defn/c replace (lat p n)\n"
    "  ;; Note: the interpreter doesn't support capturing an enclosing "
    "function's\n"
    "  ;; arguments, hence the let binding. Obviously, this is inconvenient in "
    "some\n"
    "  ;; cases, but it's not that bad.\n"
    "  (let ((pred p)\n"
    "        (newv n))\n"
    "    (map (lambda (v)\n"
    "          (if (pred v)\n"
    "              newv\n"
    "              v))\n"
    "         lat)))\n"
    "\n"
    "(defn/c curry (fn)\n"
    "  (let ((func fn)\n"
    "        (args (cdr $V)))\n"
    "    (lambda ()\n"
    "      (apply func (append args $V)))))\n"
    "\n"
    ";; Return a predicate that returns true if its argument equals the "
    "supplied value.\n"
    ";; e.g.: ((equalto? 2) 2) -> true\n"
    "(defn/c equalto? (pred)\n"
    "  (curry equal pred))\n"
    "\n"
    ";; As useful as an equalto? predicate is, often you want to know if an "
    "element\n"
    ";; of a sublist is equalto a value.\n"
    "(defn/c pos-equalto? (pos pred)\n"
    "  (let ((p pred)\n"
    "        (n pos))\n"
    "    (lambda (lat)\n"
    "      (equal p (get lat n)))))\n"
    "\n"
    "(defn/c car-equalto? (v)\n"
    "  (let ((val v))\n"
    "    (pos-equalto? 0 v)))\n"
    "\n"
    "(defn/c notequal? (val)\n"
    "  (let ((v val))\n"
    "    (lambda (o)\n"
    "      (not (equal o v)))))\n"
    "";


int main(int argc, char** argv)
{
    Platform pfrm;

    lisp::init({});

    lisp::BasicCharSequence ut_seq(utilities);
    lisp::dostring(ut_seq, [](lisp::Value& err) {});

    lisp::set_var("newline", lisp::make_function([](int argc) {
                      std::cout << std::endl;
                      return L_NIL;
                  }));

    lisp::set_var("put", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, string);
                      std::cout << L_LOAD_STRING(0);
                      return L_NIL;
                  }));

    lisp::set_var("print", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      Printer p;
                      format(lisp::get_op0(), p);
                      return L_NIL;
                  }));

    lisp::set_var("getline", lisp::make_function([](int argc) {
                      std::string line;
                      if (std::getline(std::cin, line)) {
                          return lisp::make_string(line.c_str());
                      } else {
                          return L_NIL;
                      }
                  }));

    lisp::set_var("eval-file", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, string);
                      std::ifstream t(L_LOAD_STRING(0));
                      std::stringstream buffer;
                      buffer << t.rdbuf();
                      auto str = buffer.str();
                      lisp::BasicCharSequence seq(str.c_str());
                      return lisp::dostring(seq, [](lisp::Value& err) {
                          Printer p;
                          lisp::format(&err, p);
                          std::cout << std::endl;
                          exit(EXIT_FAILURE);
                      });
                  }));

    for (int i = 1; i < argc; ++i) {
        auto path = argv[i];
        std::ifstream t(path);
        std::stringstream buffer;
        buffer << t.rdbuf();
        auto str = buffer.str();
        lisp::BasicCharSequence seq(str.c_str());
        lisp::dostring(seq, [](lisp::Value& err) {
            Printer p;
            lisp::format(&err, p);
            std::cout << std::endl;
            exit(EXIT_FAILURE);
        });
    }

    return EXIT_SUCCESS;
}
