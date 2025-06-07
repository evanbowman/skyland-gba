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

#include "lisp.hpp"


namespace lisp
{


using LocalVariableOffset = std::pair<u8, u8>;

Optional<LocalVariableOffset> __find_local(const char* intern_str);
Value* __get_local(LocalVariableOffset off);


Value* get_var_stable(const char* intern_str);


bool is_boolean_true(Value* val);


Value* make_bytecode_function(Value* bytecode);


void lexical_frame_push();
void lexical_frame_pop();
void lexical_frame_store(Value* kvp);


NativeInterface::LookupResult __load_builtin(const char* name);


} // namespace lisp
