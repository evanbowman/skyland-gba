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

#include "lisp.hpp"


namespace lisp::debug
{


enum class Action {
    step,
    step_over,
    resume,
};


enum class Interrupt {
    breakpoint,
    step,
    watchpoint,
    enter_compiled_function,
};


static constexpr const int debug_fn_size = 2 * sizeof(void*);
using DebugHandler = ::Function<debug_fn_size, Action(Interrupt, Value*)>;


void register_debug_handler(DebugHandler debug_handler);
void register_symbol_breakpoint(Value* symbol);
void delete_symbol_breakpoint(Value* symbol);

void register_symbol_watchpoint(Value* symbol);
void delete_symbol_watchpoint(Value* symbol);

bool is_symbol_breakpoint(const char* str);


Value* get_watchpoints_list();



struct LocalVar
{
    const char* name_;
    Value* value_;
};


Vector<LocalVar> get_locals();


} // namespace lisp::debug
