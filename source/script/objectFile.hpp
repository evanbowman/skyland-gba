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

    struct Header
    {
        enum class Flag
        {
            relocatable = (1 << 0),
        };

        bool has_flag(Flag f) const
        {
            return flags_.get() & (u16)f;
        }

        void set_flag(Flag f)
        {
            auto flags = flags_.get();
            flags |= (u16)f;
            flags_.set(flags);
        }

        host_u16 flags_;
        Fingerprint fingerprint_;
        host_u16 definition_count_;
    };

    struct RelocationTableInfo
    {
        host_u32 offset_;
    };


    ObjectFile(const Fingerprint& f, bool relocatable = false);


    void append(Symbol::SymtabIndex sym, Function& fn);


    void save(const char* path);


    Value* load(const Fingerprint& f, const char* path);


    static bool disassemble(const char* path, Vector<char>& output);


    struct Definition
    {
        HostInteger<Symbol::SymtabIndex> sym_;
        Function::Signature sig_;
    };


    using RelocationTable = Vector<StringBuffer<32>>;


    static Symbol::SymtabIndex link_symbol(Symbol::SymtabIndex input,
                                           RelocationTable& table,
                                           bool relocatable);


private:
    void append(void* data, u32 len);

    static void load_file_contents(Vector<char>& output, const char* path);

    Symbol::SymtabIndex store_symbol(Symbol::SymtabIndex input);

    Vector<char> bytes_;
    RelocationTable relocation_table_;
    u16 definition_count_ = 0;
    bool relocatable_;
};


} // namespace lisp
