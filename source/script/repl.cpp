////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "lisp.hpp"
#include "platform/platform.hpp"


#include <fstream>
#include <iostream>



class Printer : public lisp::Printer {
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
    "(macro repeat (N BODY)\n"
    " `(map (lambda ,@BODY) (range 0 ,N)))\n"
    "\n"
    "\n"
    ";; Some useful macros for defining functions\n"
    "\n"
    ";; Defines a function.\n"
    "(macro defn (NAME REST) `(setq ,NAME (require-args (lambda ,@(cdr REST)) "
    ",(caar REST))))\n"
    ";; Defines a bytecode-compiled function.\n"
    "(macro defn/c (NAME REST) `(setq ,NAME (require-args (compile (lambda "
    ",@(cdr REST))) ,(caar REST))))\n"
    "\n"
    "(macro += (NAME VAL)\n"
    " `(setq ,NAME (+ ,NAME ,@VAL)))\n"
    "\n"
    "(macro setq (NAME EXPR) `(set ,(cons $q NAME) ,@EXPR))\n"
    "\n"
    "(macro when (EXPR BODY) `(if ,EXPR (progn ,@BODY)))\n"
    "(macro unless (EXPR BODY) `(if (not ,EXPR) (progn ,@BODY)))\n"
    "\n"
    "\n"
    ";; A shortcut for defining functions with named arguments. e.g.:\n"
    ";; (defun foo (a b c)\n"
    ";;   (+ a b c))\n"
    ";; Currently disabled, as named arguments require more memory and I "
    "personally\n"
    ";; don't mind refering to arguments by number.\n"
    ";; (macro defun (NAME ARGS BODY) `(set ,(cons $q NAME) (fn ,ARGS "
    ",@BODY)))\n"
    "\n"
    "\n"
    ";; Because we're running lisp in an embedded system (a gameboy) with "
    "limited\n"
    ";; memory, we need to be really careful about symbol table usage, which "
    "is why,\n"
    ";; traditionally, we only support numbered arguments for lambdas. But "
    "this\n"
    ";; function macro allows you to declare functions with named arguments:\n"
    "(macro fn (ARGS BODY)\n"
    " (if (not ARGS)\n"
    "     `(lambda ,@BODY)\n"
    "   `(lambda\n"
    "     (let ,((lambda\n"
    "             (if (not $0)\n"
    "                 $1\n"
    "               ((this)\n"
    "                (cdr $0)\n"
    "                (cons (list (car $0) (symbol (string \"$\" $2))) $1)\n"
    "                (+ $2 1))))\n"
    "            ARGS nil 0)\n"
    "       ,@BODY))))\n"
    "\n"
    "\n"
    "(macro progn (BODY)\n"
    " `(let () ,@BODY))\n"
    "\n"
    "\n"
    "(defn/c acons [3]\n"
    "  (cons (cons $0 $1) $2))\n"
    "\n"
    "\n"
    "(defn/c assoc [2]\n"
    "  (let ((temp $0))\n"
    "    (get (filter (lambda (equal (car $0) temp))\n"
    "                 $1)\n"
    "         0)))\n"
    "\n"
    "\n"
    "\n"
    "(defn append [2]\n"
    "  ;; Not the most efficient way to implement append, but this "
    "implementation\n"
    "  ;; with unquote-splicing is quite compact.\n"
    "  `(,@$0 ,@$1))\n"
    "\n"
    "\n"
    "(defn/c push [2]\n"
    "  (set $0 (cons $1 (eval $0))))\n"
    "\n"
    "\n"
    "(setq bisect\n"
    "     (let ((impl (compile\n"
    "                  (lambda\n"
    "                    (if (not $1)\n"
    "                        (cons (reverse $2) $0)\n"
    "                      (if (not (cdr $1))\n"
    "                          (cons (reverse $2) $0)\n"
    "                        ((this)\n"
    "                         (cdr $0)\n"
    "                         (cdr (cdr $1))\n"
    "                         (cons (car $0) $2))))))))\n"
    "       (lambda (impl $0 $0 '()))))\n"
    "\n"
    "\n"
    "(defn/c merge [3]\n"
    "  (cond\n"
    "   ((not $0) $1)\n"
    "   ((not $1) $0)\n"
    "   (($2 (car $0) (car $1))\n"
    "    (cons (car $0) ((this) (cdr $0) $1 $2)))\n"
    "   (true (cons (car $1) ((this) $0 (cdr $1) $2)))))\n"
    "\n"
    "\n"
    "(defn/c sort [2]\n"
    "  (if (not (cdr $0))\n"
    "      $0\n"
    "    (let ((temp (bisect $0)))\n"
    "      (merge (sort (car temp) $1)\n"
    "             (sort (cdr temp) $1)\n"
    "             $1))))\n"
    "\n"
    "\n"
    ";; While suboptimal, these functions have the benefit of being small.\n"
    "(defn/c min [1] (car (sort $0 <)))\n"
    "(defn/c max [1] (car (sort $0 >)))\n"
    "\n"
    "(defn/c replace [3]\n"
    "  ;; (lat predicate new-value)\n"
    "  (let ((pred $1)\n"
    "        (newv $2))\n"
    "    (map\n"
    "     (lambda\n"
    "       (if (pred $0)\n"
    "           newv\n"
    "         $0))\n"
    "     $0)))\n"
    "\n"
    "(defn/c curry [1]\n"
    "  (let ((func $0)\n"
    "        (args (cdr $V)))\n"
    "    (lambda\n"
    "      (apply func (append args $V)))))\n"
    "\n"
    ";; Return a predicate that returns true if its argument equals the "
    "supplied value.\n"
    ";; e.g.: ((equalto? 2) 2) -> true\n"
    "(defn/c equalto? [1]\n"
    "  (curry equal $0))\n"
    "";


int main(int argc, char** argv)
{
    Platform pfrm;

    lisp::init();

    lisp::BasicCharSequence ut_seq(utilities);
    lisp::dostring(ut_seq, [](lisp::Value& err) {});

    const char* prompt = ">> ";

    std::string line;
    std::cout << prompt;
    while (std::getline(std::cin, line)) {
        if (not line.empty()) {
            lisp::BasicCharSequence seq(line.c_str());
            lisp::read(seq);
            Printer p;
            auto result = lisp::get_op(0);
            lisp::pop_op();
            lisp::eval(result);
            result = lisp::get_op(0);
            lisp::pop_op();
            format(result, p);
            std::cout << std::endl << prompt;
        } else {
            std::cout << prompt;
        }
    }
}
