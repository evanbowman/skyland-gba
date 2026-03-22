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

#include "debug.hpp"
#include "lisp.hpp"


namespace lisp
{


using LocalVariableOffset = std::pair<u8, u8>;

Optional<LocalVariableOffset> __find_local(const char* intern_str);
Value* __get_local(LocalVariableOffset off);


Value* get_var_stable(const char* intern_str);


bool is_boolean_true(Value* val);


Value* make_bytecode_function(Value* bytecode);
Value* make_symtab_symbol(int index);
const char* load_from_symtab(int offset);
static constexpr const u32 symtab_stride = 32;

Optional<u16> symbol_indexof(const char* string);


void lexical_frame_push();
void lexical_frame_pop();
void lexical_frame_store(Value* kvp);


NativeInterface::LookupResult __load_builtin(const char* name);
const char* nameof(Function::CPP_Impl impl);


const char* repr_arg_type(u8 sig_type);


Value*& get_bytecode_buffer();


void collect_value(Value* value);


Optional<u16> get_symtab_index(Symbol& sym);


} // namespace lisp
