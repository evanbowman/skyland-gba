////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "lisp.hpp"
#include "platform/platform.hpp"


#include <fstream>
#include <iostream>


static lisp::Value* function_test()
{
    using namespace lisp;

    set_var("double", make_function([](int argc) {
                L_EXPECT_ARGC(argc, 1);
                L_EXPECT_OP(0, integer);

                return make_integer(get_op(0)->integer().value_ * 2);
            }));

    push_op(make_integer(48));
    funcall(get_var("double"), 1);

    L_EXPECT_OP(0, integer);

    if (get_op(0)->integer().value_ not_eq 48 * 2) {
        std::cout << "funcall test result check failed!" << std::endl;
        return L_NIL;
    }

    // if (bound_context->operand_stack_.size() not_eq 1) {
    //     std::cout << "operand stack size check failed!" << std::endl;
    //     return L_NIL;
    // }


    pop_op();

    std::cout << "funcall test passed!" << std::endl;

    return L_NIL;
}


static lisp::Value* arithmetic_test()
{
    using namespace lisp;

    push_op(make_integer(48));
    push_op(make_integer(96));
    funcall(get_var("-"), 2);

    L_EXPECT_OP(0, integer);

    if (get_op(0)->integer().value_ not_eq 48 - 96) {
        std::cout << "bad arithmetic!" << std::endl;
        return L_NIL;
    }

    std::cout << "arithmetic test passed!" << std::endl;

    return L_NIL;
}


static void intern_test()
{
    auto initial = lisp::intern("blah");
    if (str_cmp("blah", initial) not_eq 0) {
        std::cout << "interpreter intern failed" << std::endl;
        return;
    }

    // Intern some other junk. We want to re-intern the initial string (above),
    // and make sure that we do not receive a non-matching pointer, which would
    // indicate that we somehow have two copies of the string in the intern
    // table.
    if (str_cmp(lisp::intern("dskjflfs"), "dskjflfs") not_eq 0) {
        std::cout << "intern failed" << std::endl;
    }

    if (lisp::intern("blah") not_eq initial) {
        std::cout << "string intern leak" << std::endl;
        return;
    }

    std::cout << "intern test passed!" << std::endl;
}

class Printer : public lisp::Printer {
public:
    void put_str(const char* str) override
    {
        std::cout << str;
    }
};


void do_tests()
{
    auto lat = lisp::make_list(9);

    lisp::set_var("L", lat);
    lisp::set_list(lat, 4, lisp::make_integer(12));

    Printer p;
    lisp::format(lisp::get_list(lisp::get_var("L"), 4), p);

    intern_test();
    function_test();
    arithmetic_test();
}


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
    "(macro defn (NAME BODY) `(setq ,NAME (lambda ,@BODY)))\n"
    ";; Defines a bytecode-compiled function.\n"
    "(macro defn/c (NAME BODY) `(setq ,NAME (compile (lambda ,@BODY))))\n"
    "\n"
    "(macro += (NAME VAL)\n"
    " `(setq ,NAME (+ ,NAME ,@VAL)))\n"
    "\n"
    "(macro setq (NAME EXPR)\n"
    " `(set ,(cons $q NAME) ,@EXPR))\n"
    "\n"
    "\n"
    ";; For our onscreen keyboard, which has no + key\n"
    "(setq add +)\n"
    "\n"
    "\n"
    ";; Because we're running lisp in an embedded system (a gameboy) with "
    "limited\n"
    ";; memory, we need to be really careful about symbol table usage, which "
    "is why,\n"
    ";; traditionally, we only support numbered arguments for lambdas. But "
    "this\n"
    ";; function macro allows you to declare functions with named arguments:\n"
    "\n"
    "(macro defun (NAME ARGS BODY) `(set ,(cons $q NAME) (fn ,ARGS ,@BODY)))"
    "\n"
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
    "(defn/c acons\n"
    "  (cons (cons $0 $1) $2))\n"
    "\n"
    "\n"
    "(defn/c assoc\n"
    "  (let ((temp $0))\n"
    "    (get (filter (lambda (equal (car $0) temp))\n"
    "                 $1)\n"
    "         0)))\n"
    "\n"
    "\n"
    "\n"
    "(defn append\n"
    "  ;; Not the most efficient way to implement append, but this "
    "implementation\n"
    "  ;; with unquote-splicing is quite compact.\n"
    "  `(,@$0 ,@$1))\n"
    "\n"
    "\n"
    "(defn/c push\n"
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
    "(defn/c merge\n"
    "  (cond\n"
    "   ((not $0) $1)\n"
    "   ((not $1) $0)\n"
    "   (($2 (car $0) (car $1))\n"
    "    (cons (car $0) ((this) (cdr $0) $1 $2)))\n"
    "   (true (cons (car $1) ((this) $0 (cdr $1) $2)))))\n"
    "\n"
    "\n"
    "(defn/c sort\n"
    "  (if (not (cdr $0))\n"
    "      $0\n"
    "    (let ((temp (bisect $0)))\n"
    "      (merge (sort (car temp) $1)\n"
    "             (sort (cdr temp) $1)\n"
    "             $1))))\n"
    "\n"
    "\n"
    ";; While suboptimal, these functions have the benefit of being small.\n"
    "(defn/c min (car (sort $0 <)))\n"
    "(defn/c max (car (sort $0 >)))\n"
    "";


int main(int argc, char** argv)
{
    Platform pfrm;

    lisp::init();

    // lisp::BasicCharSequence ut_seq(utilities);
    // lisp::dostring(ut_seq, [](lisp::Value& err) {});

    const char* prompt = ">> ";

    std::string line;
    std::cout << prompt;
    while (std::getline(std::cin, line)) {
        if (not line.empty()) {
            lisp::BasicCharSequence seq(line.c_str());
            lisp::read(seq);
            Printer p;
            auto result = lisp::get_op(0);
            // format(result, p);
            // std::cout << std::endl;
            lisp::pop_op();
            lisp::eval(result);
            result = lisp::get_op(0);
            lisp::pop_op();
            format(result, p);
            // std::cout << "stack size: "
            //           << lisp::bound_context->operand_stack_.size()
            //           << ", object pool: "
            //           << lisp::bound_context->memory_.remaining()
            //           << ", intern mem: " << lisp::bound_context->string_intern_pos_
            //           << std::endl;
            std::cout << std::endl << prompt;
        } else {
            std::cout << prompt;
        }
    }
}
