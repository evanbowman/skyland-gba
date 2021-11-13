#include "lisp.hpp"
#include "bytecode.hpp"
#include "platform/platform.hpp"
#include <fstream>
#include <sstream>
#include <iostream>


class Printer : public lisp::Printer {
public:
    void put_str(const char* str) override
    {
        std::cout << str;
    }
};


int main(int argc, char** argv)
{
    if (argc not_eq 2) {
        puts("usage: LISPC <filename>");
        return 1;
    }

    Platform platform;

    lisp::init(platform);

    std::ifstream t(argv[1]);
    std::stringstream buffer;
    buffer << t.rdbuf();

    // Wrap code with (lambda ... ). The compile function expects a lambda as an
    // argument.
    lisp::read(("(lambda " + buffer.str() + ")").c_str()); // result on operand stack
    lisp::eval(lisp::get_op(0));

    lisp::funcall(lisp::get_var("compile"), 1);

    if (lisp::get_op(0)->type() == lisp::Value::Type::error) {
        puts("compiler error!");
        Printer p;
        lisp::format(lisp::get_op(0), p);
        return 1;
    }

    std::ofstream out("a.out", std::ios::binary);
    // TODO: ... write output ...

    lisp::funcall(lisp::get_var("disassemble"), 1);

    return 0;
}
