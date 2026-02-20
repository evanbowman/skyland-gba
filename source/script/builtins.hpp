////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "lisp.hpp"


namespace lisp
{


Value* builtin_read(int argc);
Value* builtin_eval(int argc);
Value* builtin_apply(int argc);

Value* builtin_list(int argc);

Value* builtin_cons(int argc);
Value* builtin_car(int argc);
Value* builtin_cdr(int argc);
Value* builtin_cddr(int argc);
Value* builtin_cadr(int argc);
Value* builtin_cdar(int argc);
Value* builtin_caar(int argc);

Value* builtin_identity(int argc);

Value* builtin_signature(int argc);
Value* builtin_require_args(int argc);
Value* builtin_rot13(int argc);

Value* builtin_string(int argc);
Value* builtin_split(int argc);
Value* builtin_string_to_bytes(int argc);
Value* builtin_bytes_to_string(int argc);
Value* builtin_slice(int argc);
Value* builtin_format(int argc);
Value* builtin_string_explode(int argc);
Value* builtin_string_assemble(int argc);

Value* builtin_symbol(int argc);

Value* builtin_add(int argc);
Value* builtin_subtract(int argc);
Value* builtin_multiply(int argc);
Value* builtin_divide(int argc);
Value* builtin_mod(int argc);
Value* builtin_rationalize(int argc);
Value* builtin_abs(int argc);
Value* builtin_int_to_bytes(int argc);
Value* builtin_bytes_to_int(int argc);
Value* builtin_toint(int argc);
Value* builtin_tofloat(int argc);
Value* builtin_range(int argc);
Value* builtin_incr(int argc);
Value* builtin_decr(int argc);
Value* builtin_hex(int argc);

Value* builtin_bit_and(int argc);
Value* builtin_bit_or(int argc);
Value* builtin_bit_xor(int argc);
Value* builtin_bit_not(int argc);
Value* builtin_bit_shift_right(int argc);
Value* builtin_bit_shift_left(int argc);

Value* builtin_wrap(int argc);
Value* builtin_unwrap(int argc);

Value* builtin_sort(int argc);
Value* builtin_flatten(int argc);
Value* builtin_reverse(int argc);
Value* builtin_filter(int argc);
Value* builtin_map(int argc);
Value* builtin_foreach(int argc);
Value* builtin_comp_less_than(int argc);
Value* builtin_comp_greater_than(int argc);
Value* builtin_find(int argc);
Value* builtin_difference(int argc);
Value* builtin_union(int argc);
Value* builtin_length(int argc);
Value* builtin_fill(int argc);
Value* builtin_get(int argc);
Value* builtin_logical_not(int argc);
Value* builtin_comp_equal(int argc);

Value* builtin_databuffer(int argc);
Value* builtin_buffer_read(int argc);
Value* builtin_buffer_write(int argc);

Value* builtin_compile(int argc);
Value* builtin_disassemble(int argc);
Value* builtin_nameof(int argc);
Value* builtin_apropos(int argc);

Value* builtin_is_odd(int argc);
Value* builtin_is_wrapped(int argc);
Value* builtin_is_string(int argc);
Value* builtin_is_databuffer(int argc);
Value* builtin_is_symbol(int argc);
Value* builtin_is_error(int argc);
Value* builtin_is_lambda(int argc);
Value* builtin_is_pair(int argc);
Value* builtin_is_float(int argc);
Value* builtin_is_int(int argc);
Value* builtin_is_ratio(int argc);
Value* builtin_is_nil(int argc);
Value* builtin_is_list(int argc);
Value* builtin_is_boolean(int argc);
Value* builtin_type(int argc);

Value* builtin_profile(int argc);

Value* builtin_error(int argc);
Value* builtin_error_info(int argc);

Value* builtin_breakpoint_reg(int argc);
Value* builtin_breakpoint_unreg(int argc);
Value* builtin_watchpoint_reg(int argc);
Value* builtin_watchpoint_unreg(int argc);

} // namespace lisp
