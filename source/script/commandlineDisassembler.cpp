////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "bytecode.hpp"
#include "lisp.hpp"
#include "platform/platform.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>


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
