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


// Purpose:
// Eventually, I want to create DLC for Skyland, by encoding lisp scripts onto
// ereader cards. As bytecode is more compact than plain-text lisp code, I
// created this command-line version of the interpreter's bytecode compiler.
// Basically, this code just loads lisp code from a file, and performs:
// (compile (eval (read <string>)))
// Then, we swap out some bytecode related to symbol loading, generate a symbol
// table, and export the bytecode to a file.


class Printer : public lisp::Printer
{
public:
    void put_str(const char* str) override
    {
        std::cout << str;
    }
};


namespace lisp
{
const char* symbol_from_offset(u16 offset);
}


using SymbolTable = std::vector<std::string>;


// Our bytecode references symbols via integer offsets into the interpreter's
// string intern table. To make bytecode portable, and not dependent on the
// specific positions of interned symbols, we swap all bytecode instructions
// that reference symbol offsets with special bytecodes that refer instead to a
// symbol relocation table, consisting of all symbols referenced by the compiled
// code. The first time that the vm sees a relocation instruction, it swaps the
// instruction with a regular PUSH_SYMBOL instruction with the corrected symbol
// offset, so that symbol lookup isn't significantly slower in tight loops.
SymbolTable make_relocatable(ScratchBuffer& bytecode)
{
    int depth = 0;
    int index = 0;

    SymbolTable symbol_table;

    auto register_symbol = [&](const char* symbol_name) {
        for (size_t i = 0; i < symbol_table.size(); ++i) {
            if (symbol_table[i] == symbol_name) {
                return i;
            }
        }
        auto ret = symbol_table.size();
        symbol_table.push_back(symbol_name);
        return ret;
    };

    while (true) {
        auto inst = lisp::instruction::load_instruction(bytecode, index);

        switch (inst->op_) {
        case lisp::instruction::PushLambda::op():
            ++depth;
            ++index;
            break;

        case lisp::instruction::Ret::op():
            if (depth == 0) {
                int bytes_used = 0;
                for (auto& symbol : symbol_table) {
                    bytes_used += symbol.length() + 1;
                    std::cout << symbol << std::endl;
                }
                std::cout << "symbol table bytes: " << bytes_used << std::endl;
                return symbol_table;
            }
            --depth;
            ++index;
            break;

        case lisp::instruction::LoadVar::op(): {
            auto symbol_offset =
                ((lisp::instruction::LoadVar*)inst)->name_offset_.get();
            auto sym_num =
                register_symbol(lisp::symbol_from_offset(symbol_offset));
            lisp::instruction::LoadVarRelocatable reloc;
            reloc.name_offset_.set(sym_num);
            reloc.header_.op_ = lisp::instruction::LoadVarRelocatable::op();
            memcpy(inst, &reloc, sizeof reloc);
            ++index;
            break;
        }

        case lisp::instruction::PushSymbol::op(): {
            auto symbol_offset =
                ((lisp::instruction::PushSymbol*)inst)->name_offset_.get();
            auto sym_num =
                register_symbol(lisp::symbol_from_offset(symbol_offset));
            lisp::instruction::PushSymbolRelocatable reloc;
            reloc.name_offset_.set(sym_num);
            reloc.header_.op_ = lisp::instruction::PushSymbolRelocatable::op();
            memcpy(inst, &reloc, sizeof reloc);
            ++index;
            break;
        }

        case lisp::instruction::LexicalDef::op(): {
            auto symbol_offset =
                ((lisp::instruction::LexicalDef*)inst)->name_offset_.get();
            auto sym_num =
                register_symbol(lisp::symbol_from_offset(symbol_offset));
            lisp::instruction::LexicalDefRelocatable reloc;
            reloc.name_offset_.set(sym_num);
            reloc.header_.op_ = lisp::instruction::LexicalDefRelocatable::op();
            memcpy(inst, &reloc, sizeof reloc);
            ++index;
            break;
        }

        default:
            ++index;
            break;
        }
    }
}


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

    auto wrapped = ("(lambda " + buffer.str() + ")");
    lisp::BasicCharSequence seq(wrapped.c_str());
    lisp::read(seq); // result on operand stack
    lisp::eval(lisp::get_op(0));

    lisp::funcall(lisp::get_var("compile"), 1);

    if (lisp::get_op(0)->type() == lisp::Value::Type::error) {
        puts("compiler error!");
        Printer p;
        lisp::format(lisp::get_op(0), p);
        return 1;
    }

    // lisp::push_op(lisp::get_op(0));
    // lisp::funcall(lisp::get_var("disassemble"), 1);
    // lisp::pop_op();

    auto code_buffer = lisp::get_op(0)
                           ->function()
                           .bytecode_impl_.databuffer()
                           ->databuffer()
                           .value();

    auto sym = make_relocatable(*code_buffer);

    int used = SCRATCH_BUFFER_SIZE - 1;
    for (; used > 0; --used) {
        if ((lisp::Opcode)code_buffer->data_[used] not_eq
            lisp::instruction::Fatal::op()) {
            ++used;
            break;
        }
    }

    std::ofstream out("a.out", std::ios::binary);

    lisp::Module::Header header;
    header.symbol_count_.set(sym.size());
    header.bytecode_length_.set(used);
    out.write((const char*)&header, sizeof header);

    for (auto& symbol : sym) {
        out.write(symbol.c_str(), symbol.length());
        out.write("\0", 1);
    }

    out.write(code_buffer->data_, used);

    // lisp::funcall(lisp::get_var("disassemble"), 1);


    return 0;
}
