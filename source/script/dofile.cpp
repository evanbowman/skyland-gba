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
    " `(map (lambda ,@BODY) (range 0 ,N)))\n"
    "\n"
    "\n"
    ";; Some useful macros for defining functions\n"
    "\n"
    ";; Defines a function.\n"
    "(macro defn (NAME REST)\n"
    "       `(safe-setfn ,(cons $q NAME)\n"
    "                    (lambda ,@(cdr REST))\n"
    "                    ,(cons $q (car REST))))\n"
    "\n"
    ";; Defines a bytecode-compiled function.  You should only compile "
    "long-lived\n"
    ";; functions, because bytecode cannot be deallocated. At the same time, "
    "bytecode\n"
    ";; takes up less space than non-compiled functions.\n"
    "(macro defn/c (NAME REST)\n"
    "       `(safe-setfn ,(cons $q NAME)\n"
    "                    (compile (lambda ,@(cdr REST)))\n"
    "                    ,(cons $q (car REST))))\n"
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
    ";; NOTE: for historical reasons, lambdas do not include syntax for "
    "specifying an\n"
    ";; argument count, as this scripting language only supports numbered "
    "positional\n"
    ";; arguments. Require-args was a safety feature added retrospectively, "
    "and I've\n"
    ";; hacked it into the function defintion macros.\n"
    "(setq safe-setfn\n"
    "      (require-args\n"
    "       (compile\n"
    "        (lambda\n"
    "          ;; safe-setfn is responsible for validating the format of data "
    "passed\n"
    "          ;; to defn, and setting the function in the environment.\n"
    "          ;;\n"
    "          ;; Make sure that the user remembered to specify an argument "
    "count\n"
    "          ;; when using one of the defn macros:\n"
    "          (when (or (not (pair? $2))\n"
    "                    (not (int? (car $2)))\n"
    "                    (cdr $2)) ;; b/c arg count must be a list with one "
    "element\n"
    "            (fatal (string $0 \": invalid defn, missing argc\")))\n"
    "          (set $0 (require-args $1 (car $2)))))\n"
    "       3))\n"
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
    "(defn/c lookup [2]\n"
    "  (let ((kvp (assoc $0 $1)))\n"
    "    (if kvp (cdr kvp))))\n"
    "\n"
    "\n"
    "(defn append [2]\n"
    "  ;; Not the most efficient way to implement append, but this "
    "implementation\n"
    "  ;; with unquote-splicing is quite compact.\n"
    "  `(,@$0 ,@$1))\n"
    "\n"
    "(defn/c gen [2]\n"
    "  (map $0 (range $1)))\n"
    "\n"
    "(defn/c push [2]\n"
    "  (set $0 (cons $1 (eval $0))))\n"
    "\n"
    "\n"
    "(defn/c push-set [2]\n"
    "  (let ((tmp (cons $1 (eval $0))))\n"
    "    (set $0 (union tmp tmp))))\n"
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
    "    (let ((len (length $0)))\n"
    "      (merge ((this) (slice $0 0 (/ len 2)) $1)\n"
    "             ((this) (slice $0 (/ len 2)) $1)\n"
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
    "\n"
    "(defn/c notequal? [1]\n"
    "  (let ((v $0))\n"
    "    (lambda\n"
    "      (not (equal $0 v)))))\n"
    "";


int main(int argc, char** argv)
{
    Platform pfrm;

    lisp::init();

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
