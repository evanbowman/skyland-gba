////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "containers/vector.hpp"
#include "lisp.hpp"
#include "number/endian.hpp"


namespace lisp
{


class ObjectFile
{
public:
    struct Fingerprint
    {
        u8 mem_[8];
    };


    ObjectFile(const Fingerprint& f);


    void append(Symbol::SymtabIndex sym, Function& fn);


    void save(const char* path);


    Value* load(const Fingerprint& f, const char* path);


    static bool disassemble(const char* path, Vector<char>& output);


    struct Definition
    {
        HostInteger<Symbol::SymtabIndex> sym_;
        Function::Signature sig_;
    };


private:
    void append(void* data, u32 len);

    void load_file_contents(const char* path);


    Vector<char> bytes_;
    u16 definition_count_ = 0;
};


} // namespace lisp
