#include "lisp.hpp"
#include "bytecode.hpp"
#include "platform/platform.hpp"
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>
#include <string>



int main(int argc, char** argv)
{
    std::ifstream stream("a.out", std::ios::in | std::ios::binary);
    std::vector<uint8_t> contents((std::istreambuf_iterator<char>(stream)),
                                  std::istreambuf_iterator<char>());

    Platform platform;

    lisp::init(platform);

    lisp::load_module((lisp::Module*)contents.data());
    lisp::funcall(lisp::get_var("disassemble"), 1);
}
