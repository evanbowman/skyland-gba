////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "number/endian.hpp"
#include "number/int.h"
#include "platform/libc.hpp"
#include "platform/scratch_buffer.hpp"
#include "value.hpp"


// NOTE: Bytecode is not portable, nor is it intended to be.


namespace lisp
{


using Opcode = u8;


namespace instruction
{


template <typename T> struct PackedData
{
    T get()
    {
        T result;
        memcpy(&result, data_, sizeof(data_));
        return result;
    }

    void set(T val)
    {
        memcpy(data_, &val, sizeof(data_));
    }

    u8 data_[sizeof(T)];
};


using UnalignedPtr = PackedData<const char*>;
using PackedFloat = PackedData<float>;


struct Header
{
    Opcode op_;
};


struct Fatal
{
    Header header_;

    static const char* name()
    {
        return "FATAL";
    }

    static constexpr Opcode op()
    {
        // Do not replace the fatal opcode with anything else, this is meant to
        // prevent buffer overruns in the event of errors. (The interpreter
        // initializes all slabs of by bytecode memory to zero.)
        return 0;
    }
};


// NOTE: in practice, most symbols are loaded from a symbol table generated when
// the scripts are built. For runtime symbols, we need a separate instruction
// listing, LOAD_VAR_RT.
struct LoadVarRT
{
    Header header_;
    UnalignedPtr ptr_;
    u8 pad_;

    static const char* name()
    {
        return "LOAD_VAR_RT";
    }

    static constexpr Opcode op()
    {
        return 1;
    }
};


struct PushNil
{
    Header header_;

    static const char* name()
    {
        return "PUSH_NIL";
    }

    static constexpr Opcode op()
    {
        return 2;
    }
};


struct PushInteger
{
    Header header_;
    host_u32 value_;

    static const char* name()
    {
        return "PUSH_INTEGER";
    }

    static constexpr Opcode op()
    {
        return 3;
    }
};


struct PushSmallInteger
{
    Header header_;
    s8 value_;

    static const char* name()
    {
        return "PUSH_SMALL_INTEGER";
    }

    static constexpr Opcode op()
    {
        return 4;
    }
};


struct Push0
{
    Header header_;

    static const char* name()
    {
        return "PUSH_0";
    }

    static constexpr Opcode op()
    {
        return 5;
    }
};


struct Push1
{
    Header header_;

    static const char* name()
    {
        return "PUSH_1";
    }

    static constexpr Opcode op()
    {
        return 6;
    }
};


struct Push2
{
    Header header_;

    static const char* name()
    {
        return "PUSH_2";
    }

    static constexpr Opcode op()
    {
        return 7;
    }
};


struct PushSymbolRT
{
    Header header_;
    UnalignedPtr ptr_;

    static const char* name()
    {
        return "PUSH_SYMBOL";
    }

    static constexpr Opcode op()
    {
        return 8;
    }
};


struct PushList
{
    Header header_;
    u8 element_count_;

    static const char* name()
    {
        return "PUSH_LIST";
    }

    static constexpr Opcode op()
    {
        return 9;
    }
};


struct Funcall
{
    Header header_;
    u8 argc_;

    static const char* name()
    {
        return "FUNCALL";
    }

    static constexpr Opcode op()
    {
        return 10;
    }
};


struct Funcall1
{
    Header header_;

    static const char* name()
    {
        return "FUNCALL_1";
    }

    static constexpr Opcode op()
    {
        return 11;
    }
};


struct Funcall2
{
    Header header_;

    static const char* name()
    {
        return "FUNCALL_2";
    }

    static constexpr Opcode op()
    {
        return 12;
    }
};


struct Funcall3
{
    Header header_;

    static const char* name()
    {
        return "FUNCALL_3";
    }

    static constexpr Opcode op()
    {
        return 13;
    }
};


struct Jump
{
    Header header_;
    host_s16 offset_;

    static const char* name()
    {
        return "JUMP";
    }

    static constexpr Opcode op()
    {
        return 14;
    }
};


struct SmallJump
{
    Header header_;
    u8 offset_;

    static const char* name()
    {
        return "JUMP_SMALL";
    }

    static constexpr Opcode op()
    {
        return 15;
    }
};


struct JumpIfFalse
{
    Header header_;
    host_s16 offset_;

    static const char* name()
    {
        return "JUMP_IF_FALSE";
    }

    static constexpr Opcode op()
    {
        return 16;
    }
};


struct SmallJumpIfFalse
{
    Header header_;
    u8 offset_;

    static const char* name()
    {
        return "JUMP_SMALL_IF_FALSE";
    }

    static constexpr Opcode op()
    {
        return 17;
    }
};


struct PushLambda
{
    Header header_;
    host_u16 lambda_end_;

    static const char* name()
    {
        return "PUSH_LAMBDA";
    }

    static constexpr Opcode op()
    {
        return 18;
    }
};


struct Pop
{
    Header header_;

    static const char* name()
    {
        return "POP";
    }

    static constexpr Opcode op()
    {
        return 19;
    }
};


struct Dup
{
    Header header_;

    static const char* name()
    {
        return "DUP";
    }

    static constexpr Opcode op()
    {
        return 20;
    }
};


struct Ret
{
    Header header_;

    static const char* name()
    {
        return "RET";
    }

    static constexpr Opcode op()
    {
        return 21;
    }
};


struct MakePair
{
    Header header_;

    static const char* name()
    {
        return "MAKE_PAIR";
    }

    static constexpr Opcode op()
    {
        return 22;
    }
};


struct First
{
    Header header_;

    static const char* name()
    {
        return "CAR";
    }

    static constexpr Opcode op()
    {
        return 23;
    }
};


struct Rest
{
    Header header_;

    static const char* name()
    {
        return "CDR";
    }

    static constexpr Opcode op()
    {
        return 24;
    }
};


struct Arg
{
    Header header_;

    static const char* name()
    {
        return "ARGN";
    }

    static constexpr Opcode op()
    {
        return 27;
    }
};


struct TailCall
{
    Header header_;
    u8 argc_;

    static const char* name()
    {
        return "TAILCALL";
    }

    static constexpr Opcode op()
    {
        return 28;
    }
};


struct TailCall1
{
    Header header_;

    static const char* name()
    {
        return "TAILCALL1";
    }

    static constexpr Opcode op()
    {
        return 29;
    }
};


struct TailCall2
{
    Header header_;

    static const char* name()
    {
        return "TAILCALL2";
    }

    static constexpr Opcode op()
    {
        return 30;
    }
};


struct TailCall3
{
    Header header_;

    static const char* name()
    {
        return "TAILCALL3";
    }

    static constexpr Opcode op()
    {
        return 31;
    }
};


struct PushThis
{
    Header header_;

    static const char* name()
    {
        return "PUSH_THIS";
    }

    static constexpr Opcode op()
    {
        return 32;
    }
};


struct Arg0
{
    Header header_;

    static const char* name()
    {
        return "ARG0";
    }

    static constexpr Opcode op()
    {
        return 33;
    }
};


struct Arg1
{
    Header header_;

    static const char* name()
    {
        return "ARG1";
    }

    static constexpr Opcode op()
    {
        return 34;
    }
};


struct Arg2
{
    Header header_;

    static const char* name()
    {
        return "ARG2";
    }

    static constexpr Opcode op()
    {
        return 35;
    }
};


// By convention, we distinguish between a return from the end of a function,
// and a return from the middle of a function. Both opcodes do the same thing,
// but we want to be able to determine where a function ends, mostly because a
// unique terminating opcode at the very end of a function makes the
// disassembler easier to write, otherwise, we'd need to store the bytecode
// length.
struct EarlyRet
{
    Header header_;

    static const char* name()
    {
        return "RET";
    }

    static constexpr Opcode op()
    {
        return 36;
    }
};


struct Not
{
    Header header_;

    static const char* name()
    {
        return "NOT";
    }

    static constexpr Opcode op()
    {
        return 37;
    }
};


struct LexicalDefRT
{
    Header header_;
    UnalignedPtr ptr_;

    static const char* name()
    {
        return "LEXICAL_DEF_RT";
    }

    static constexpr Opcode op()
    {
        return 38;
    }
};


struct LexicalFramePush
{
    Header header_;

    static const char* name()
    {
        return "LEXICAL_FRAME_PUSH";
    }

    static constexpr Opcode op()
    {
        return 39;
    }
};


struct LexicalFramePop
{
    Header header_;

    static const char* name()
    {
        return "LEXICAL_FRAME_POP";
    }

    static constexpr Opcode op()
    {
        return 40;
    }
};


struct RetNil
{
    Header header_;

    static const char* name()
    {
        return "RET_NIL";
    }

    static constexpr Opcode op()
    {
        return 41;
    }
};


struct PushString
{
    Header header_;
    u8 length_;
    // u8 bytes_[length_];

    static const char* name()
    {
        return "PUSH_STRING";
    }

    static constexpr Opcode op()
    {
        return 42;
    }
};


struct EarlyRetNil
{
    Header header_;

    static const char* name()
    {
        return "RET_NIL";
    }

    static constexpr Opcode op()
    {
        return 43;
    }
};


struct RetNilIfFalse
{
    Header header_;

    static const char* name()
    {
        return "RET_NIL_IF_FALSE";
    }

    static constexpr Opcode op()
    {
        return 44;
    }
};


// NOTE: opcode 45 has been removed, and is free to use for something else.


struct PushSmallSymbol
{
    Header header_;
    u8 name_[3];

    static const char* name()
    {
        return "PUSH_SMALL_SYMBOL";
    }

    static constexpr Opcode op()
    {
        return 46;
    }
};


struct LexicalDefSmall
{
    Header header_;
    u8 name_[3];

    static const char* name()
    {
        return "LEXICAL_DEF_SMALL";
    }

    static constexpr Opcode op()
    {
        return 47;
    }
};


struct LoadVarSmall
{
    Header header_;
    u8 name_[3];

    static const char* name()
    {
        return "LOAD_VAR_SMALL";
    }

    static constexpr Opcode op()
    {
        return 48;
    }
};


struct PushFloat
{
    Header header_;
    PackedFloat f_;

    static const char* name()
    {
        return "PUSH_FLOAT";
    }

    static constexpr Opcode op()
    {
        return 50;
    }
};


// unused 51, 52, 53

// Bytecode listings 54, 55, and 56 have been removed and can be used for
// something else.

// opcode removed, 57 unused


struct PushRatio
{
    Header header_;
    host_u32 num_;
    host_u32 div_;

    static const char* name()
    {
        return "PUSH_RATIO";
    }

    static constexpr Opcode op()
    {
        return 58;
    }
};


struct Await
{
    Header header_;

    static const char* name()
    {
        return "AWAIT";
    }

    static constexpr Opcode op()
    {
        return 59;
    }
};


struct Set
{
    Header header_;

    static const char* name()
    {
        return "SET";
    }

    static constexpr Opcode op()
    {
        return 60;
    }
};


struct LoadSymtab
{
    Header header_;
    host_u16 symtab_index_;

    static const char* name()
    {
        return "LOAD_SYM";
    }

    static constexpr Opcode op()
    {
        return 61;
    }
};


struct IsEqual
{
    Header header_;

    static const char* name()
    {
        return "IS_EQUAL";
    }

    static constexpr Opcode op()
    {
        return 63;
    }
};


struct LoadVarS
{
    Header header_;
    host_u16 symtab_index_;

    static const char* name()
    {
        return "LOAD_VAR";
    }

    static constexpr Opcode op()
    {
        return 64;
    }
};


struct LoadCall1 : public LoadVarS
{
    static const char* name()
    {
        return "LOAD/CALL1";
    }

    static constexpr Opcode op()
    {
        return 65;
    }
};


struct LoadCall2 : public LoadVarS
{
    static const char* name()
    {
        return "LOAD/CALL2";
    }

    static constexpr Opcode op()
    {
        return 66;
    }
};


struct LoadCall3 : public LoadVarS
{
    static const char* name()
    {
        return "LOAD/CALL3";
    }

    static constexpr Opcode op()
    {
        return 67;
    }
};


struct LexicalDef
{
    Header header_;
    host_u16 symtab_index_;

    static const char* name()
    {
        return "LEXICAL_DEF";
    }

    static constexpr Opcode op()
    {
        return 68;
    }
};


struct SetVarSmall
{
    Header header_;
    u8 name_[3];

    static const char* name()
    {
        return "SET_VAR_SMALL";
    }

    static constexpr Opcode op()
    {
        return 69;
    }
};


struct SetVar
{
    Header header_;
    host_u16 symtab_index_;

    static const char* name()
    {
        return "SET_VAR";
    }

    static constexpr Opcode op()
    {
        return 70;
    }
};


struct SetVarRT
{
    Header header_;
    UnalignedPtr ptr_;

    static const char* name()
    {
        return "SET_VAR_RT";
    }

    static constexpr Opcode op()
    {
        return 71;
    }
};


struct LoadReg
{
    Header header_;
    u8 reg_;

    static const char* name()
    {
        return "LOAD_REG";
    }

    static constexpr Opcode op()
    {
        return 72;
    }
};


struct StoreReg
{
    Header header_;
    u8 reg_;

    static const char* name()
    {
        return "STORE_REG";
    }

    static constexpr Opcode op()
    {
        return 73;
    }
};


struct LoadCall0 : public LoadVarS
{
    static const char* name()
    {
        return "LOAD/CALL0";
    }

    static constexpr Opcode op()
    {
        return 74;
    }
};


struct ConsVar : public SetVar
{

    static const char* name()
    {
        return "CONS_VAR";
    }

    static constexpr Opcode op()
    {
        return 75;
    }
};


struct Get
{
    Header header_;

    static const char* name()
    {
        return "GET";
    }

    static constexpr Opcode op()
    {
        return 76;
    }
};


struct Add
{
    Header header_;
    u8 operands_;

    static const char* name()
    {
        return "ADD";
    }

    static constexpr Opcode op()
    {
        return 77;
    }
};


struct StoreRegKeep : public StoreReg
{
    static const char* name()
    {
        return "STORE_REG_KEEP";
    }

    static constexpr Opcode op()
    {
        return 78;
    }
};


struct Resume
{
    Header header_;

    static const char* name()
    {
        return "RESUME";
    }

    static constexpr Opcode op()
    {
        return 79;
    }
};


struct LoadReg0
{
    Header header_;

    static const char* name()
    {
        return "LOAD_REG0";
    }

    static constexpr Opcode op()
    {
        return 80;
    }
};



void disassemble(ScratchBuffer* code_buffer,
                 s32 start_offset,
                 ::Function<2 * sizeof(void*), void(const char*)> callback);


void disassemble(Value* fn,
                 ::Function<2 * sizeof(void*), void(const char*)> callback);



} // namespace instruction

} // namespace lisp
