////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "builtins.hpp"
#include "bytecode.hpp"
#include "debug.hpp"
#include "listBuilder.hpp"
#include "localization.hpp"
#include "number/ratio.hpp"
#include "rot13.hpp"
#include <limits>


namespace lisp
{


Value* alloc_value();
const char* nameof(Function::CPP_Impl impl);


Value* builtin_read(int argc)
{
    L_EXPECT_OP(0, string);
    BasicCharSequence seq(get_op0()->string().value());
    read(seq);
    auto result = get_op0();
    pop_op();
    return result;
}


Value* builtin_eval(int argc)
{
    eval(get_op0());
    auto result = get_op0();
    pop_op(); // result

    return result;
}


Value* builtin_apply(int argc)
{
    L_EXPECT_OP(0, cons);
    L_EXPECT_OP(1, function);

    auto lat = get_op0();
    auto fn = get_op1();

    int apply_argc = 0;
    while (lat not_eq get_nil()) {
        if (lat->type() not_eq Value::Type::cons) {
            return make_error(Error::Code::invalid_argument_type, lat);
        }
        ++apply_argc;
        push_op(lat->cons().car());

        lat = lat->cons().cdr();
    }

    funcall(fn, apply_argc);

    auto result = get_op0();
    pop_op();

    return result;
}


Value* builtin_list(int argc)
{
    ListBuilder list;
    for (int i = 0; i < argc; ++i) {
        auto val = get_op((argc - 1) - i);
        if (val->type() == Value::Type::error) {
            return val;
        }
        list.push_back(val);
    }
    return list.result();
}


Value* builtin_cons(int argc)
{
    auto car = get_op1();
    auto cdr = get_op0();

    if (car->type() == lisp::Value::Type::error) {
        return car;
    }

    if (cdr->type() == lisp::Value::Type::error) {
        return cdr;
    }

    return make_cons(get_op1(), get_op0());
}


Value* builtin_car(int argc)
{
    L_EXPECT_OP(0, cons);
    return get_op0()->cons().car();
}


Value* builtin_cdr(int argc)
{
    L_EXPECT_OP(0, cons);
    return get_op0()->cons().cdr();
}


Value* builtin_cddr(int argc)
{
    L_EXPECT_OP(0, cons);
    auto cdr = get_op0()->cons().cdr();
    if (cdr->type() not_eq Value::Type::cons) {
        return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                L_NIL);
    }
    return cdr->cons().cdr();
}


Value* builtin_cadr(int argc)
{
    L_EXPECT_OP(0, cons);
    auto cdr = get_op0()->cons().cdr();
    if (cdr->type() not_eq Value::Type::cons) {
        return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                L_NIL);
    }
    return cdr->cons().car();
}


Value* builtin_cdar(int argc)
{
    L_EXPECT_OP(0, cons);
    auto car = get_op0()->cons().car();
    if (car->type() not_eq Value::Type::cons) {
        return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                L_NIL);
    }
    return car->cons().cdr();
}


Value* builtin_caar(int argc)
{
    L_EXPECT_OP(0, cons);
    auto car = get_op0()->cons().car();
    if (car->type() not_eq Value::Type::cons) {
        return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                L_NIL);
    }
    return car->cons().car();
}


Value* builtin_identity(int argc)
{
    return get_op0();
}


const char* repr_arg_type(u8 sig_type)
{
    auto vt = (Value::Type)sig_type;
    if (vt == Value::Type::nil) {
        return "?";
    } else {
        return type_to_string(vt);
    }
}


Value* repr_signature(Function::Signature sig)
{
    ListBuilder list;
    list.push_back(L_SYM(repr_arg_type(sig.ret_type_)));
    list.push_back(L_SYM(repr_arg_type(sig.arg0_type_)));
    list.push_back(L_SYM(repr_arg_type(sig.arg1_type_)));
    list.push_back(L_SYM(repr_arg_type(sig.arg2_type_)));
    list.push_back(L_SYM(repr_arg_type(sig.arg3_type_)));
    return list.result();
}


Value* builtin_signature(int argc)
{
    L_EXPECT_OP(0, function);
    auto sig = get_op0()->function().sig_;
    return repr_signature(sig);
}


Value* builtin_require_args(int argc)
{
    L_EXPECT_OP(1, function);
    L_EXPECT_OP(0, integer);

    lisp::Protected result(L_NIL);
    result = alloc_value();
    result->function() = get_op1()->function();
    result->function().sig_.required_args_ = L_LOAD_INT(0);

    return (Value*)result;
}


Value* builtin_rot13(int argc)
{
    L_EXPECT_OP(0, string);
    auto str = L_LOAD_STRING(0);
    auto rotstr = allocate<StringBuffer<1000>>("rot13");
    while (*str not_eq '\0') {
        rotstr->push_back(rot13(*str));
        ++str;
    }
    return lisp::make_string(rotstr->c_str());
}


using EvalBuffer = StringBuffer<900>;


namespace
{
class EvalPrinter : public Printer
{
public:
    EvalPrinter(EvalBuffer& buffer) : buffer_(buffer)
    {
    }

    void put_str(const char* str) override
    {
        buffer_ += str;
    }

private:
    EvalBuffer& buffer_;
};
} // namespace



void format_impl(Value* value, Printer& p, int depth, bool skip_quotes = false);


Value* builtin_string(int argc)
{
    EvalBuffer b;
    EvalPrinter p(b);

    if (argc == 1 and get_op0()->hdr_.type() == Value::Type::symbol) {
        return make_string(get_op0()->symbol().name());
    }

    for (int i = argc - 1; i > -1; --i) {
        auto val = get_op(i);
        if (val->type() == Value::Type::error) {
            return val;
        } else if (val->type() == Value::Type::string) {
            p.put_str(val->string().value());
        } else {
            format_impl(val, p, 0);
        }
    }

    return make_string(b.c_str());
}


Value* builtin_split(int argc)
{
    L_EXPECT_OP(0, string);
    L_EXPECT_OP(1, string);

    const char delim = *L_LOAD_STRING(0);
    auto str = L_LOAD_STRING(1);

    ListBuilder b;

    StringBuffer<96> temp;

    while (*str not_eq '\0') {
        if (*str == delim) {
            b.push_back(make_string(temp.c_str()));
            temp.clear();
        } else {
            temp.push_back(*str);
        }
        ++str;
    }

    if (not temp.empty()) {
        b.push_back(make_string(temp.c_str()));
    }

    return b.result();
}


Value* builtin_string_to_bytes(int argc)
{
    L_EXPECT_OP(0, string);
    ListBuilder result;

    int i = 0;
    while (get_op0()->string().value()[i] not_eq '\0') {
        result.push_back(L_INT(get_op0()->string().value()[i]));
        ++i;
    }
    return result.result();
}


Value* builtin_bytes_to_string(int argc)
{
    if (not is_list(get_op0())) {
        return make_error("bytes-to-string expects a list param!");
    }

    auto temp = allocate<StringBuffer<2000>>("bytes-to-string-temp");
    l_foreach(get_op0(),
              [&temp](Value* v) { temp->push_back(v->integer().value_); });

    return make_string(temp->c_str());
}


Value* builtin_slice(int argc)
{
    int begin = 0;
    int end = 0;

    int seq = 2;
    bool until_end = false;

    if (argc == 2) {
        L_EXPECT_OP(0, integer);
        seq = 1;
        begin = L_LOAD_INT(0);
        end = std::numeric_limits<int>::max();
        until_end = true;
    } else {
        L_EXPECT_OP(0, integer);
        L_EXPECT_OP(1, integer);
        begin = L_LOAD_INT(1);
        end = L_LOAD_INT(0);
    }

    int index = 0;

    if (get_op(seq)->type() == Value::Type::cons) {

        if (until_end) {
            // In this case, we want nodes up to and including the end
            // node, therefore, we don't need to copy any of the old
            // list, we just need to iterate until the begin index and
            // return the cdr.

            auto list = get_op(seq);
            while (true) {
                if (list->type() not_eq Value::Type::cons) {
                    break;
                } else {
                    if (index >= begin) {
                        return list;
                    }
                    ++index;
                }
                list = list->cons().cdr();
            }
            return list;
        }

        ListBuilder result;

        auto list = get_op(seq);

        while (true) {
            if (list->type() not_eq Value::Type::cons) {
                break;
            } else {
                auto v = list->cons().car();
                if (index >= begin and index < end) {
                    result.push_back(v);
                } else if (index == end) {
                    break;
                }
                ++index;
            }

            list = list->cons().cdr();
        }

        return result.result();

    } else if (get_op(seq)->type() == Value::Type::string) {

        auto inp_str = L_LOAD_STRING(2);
        auto builder = allocate<StringBuffer<2000>>("lispslice");

        int index = 0;
        utf8::scan(
            [&](const utf8::Codepoint&, const char* raw, int) {
                if (index >= begin and index < end) {
                    (*builder) += raw;
                }
                ++index;
                return true;
            },
            inp_str,
            strlen(inp_str));

        return make_string(builder->c_str());

    } else {
        L_EXPECT_OP(2, cons);
        return L_NIL;
    }
}


Value* builtin_format(int argc)
{
    int fmt_arg = argc - 2;

    L_EXPECT_OP(argc - 1, string);
    auto builder = allocate<StringBuffer<1800>>("lisp-fmt");

    auto str = get_op(argc - 1)->string().value();

    while (*str not_eq '\0') {
        if (*str == '%') {
            if (fmt_arg == -1) {
                return L_NIL;
            }

            DefaultPrinter p;
            format(get_op(fmt_arg), p);
            *builder += p.data_.c_str();

            --fmt_arg;
        } else {
            builder->push_back(*str);
        }
        ++str;
    }

    return make_string(builder->c_str());
}


Value* builtin_string_explode(int argc)
{
    L_EXPECT_OP(0, string);
    ListBuilder list;

    auto str = L_LOAD_STRING(0);
    utf8::scan(
        [&](const utf8::Codepoint& cp, const char*, int) {
            list.push_back(L_INT(cp));
            return true;
        },
        str,
        strlen(str));

    return list.result();
}


Value* builtin_symbol(int argc)
{
    L_EXPECT_OP(0, string);
    return make_symbol(get_op0()->string().value());
}


Value* builtin_string_assemble(int argc)
{
    L_EXPECT_OP(0, cons);

    auto str = allocate<StringBuffer<2000>>("tempstr");

    l_foreach(get_op0(), [&](Value* val) {
        auto v = val->integer().value_;
        auto cp = (const char*)&v;
        for (int i = 0; i < 4; ++i) {
            if (cp[i]) {
                str->push_back(cp[i]);
            }
        }
    });

    return make_string(str->c_str());
}


Value* builtin_add(int argc)
{
    if (argc == 0) {
        return make_integer(0);
    }

    // Check for float contagion
    bool has_float = false;
    for (int i = 0; i < argc; ++i) {
        if (get_op(i)->type() == Value::Type::fp) {
            has_float = true;
            break;
        }
    }

    // Float path
    if (has_float) {
        Float::ValueType accum = 0.0f;
        for (int i = 0; i < argc; ++i) {
            if (get_op(i)->type() == Value::Type::fp) {
                accum += get_op(i)->fp().value_;
            } else if (get_op(i)->type() == Value::Type::integer) {
                accum += (Float::ValueType)get_op(i)->integer().value_;
            } else if (get_op(i)->type() == Value::Type::ratio) {
                auto& ratio = get_op(i)->ratio();
                s32 num = dcompr(ratio.numerator_)->integer().value_;
                Float::ValueType val = (Float::ValueType)num / ratio.divisor_;
                accum += val;
            } else {
                return make_error("arguments must be numeric");
            }
        }
        return L_FP(accum);
    }

    // Rational path - accumulate as ratio
    s32 accum_num = 0, accum_div = 1;
    for (int i = 0; i < argc; ++i) {
        s32 op_num, op_div;
        if (!to_rational(get_op(i), op_num, op_div)) {
            return make_error("arguments must be numeric");
        }

        s32 result_num, result_div;
        add_rationals(
            result_num, result_div, accum_num, accum_div, op_num, op_div);
        accum_num = result_num;
        accum_div = result_div;
        reduce_fraction(accum_num, accum_div);
    }

    return make_ratio(accum_num, accum_div);
}


Value* builtin_subtract(int argc)
{
    if (argc == 1) {
        if (get_op0()->type() == Value::Type::fp) {
            return L_FP(-L_LOAD_FP(0));
        }

        s32 num, div;
        if (!to_rational(get_op0(), num, div)) {
            return make_error("argument must be numeric");
        }

        return make_ratio(-num, div);
    }

    L_EXPECT_ARGC(argc, 2);

    // Check for float contagion
    if (get_op0()->type() == Value::Type::fp ||
        get_op1()->type() == Value::Type::fp) {

        Float::ValueType op0 =
            (get_op0()->type() == Value::Type::fp)
                ? get_op0()->fp().value_
                : (Float::ValueType)get_op0()->integer().value_;
        Float::ValueType op1 =
            (get_op1()->type() == Value::Type::fp)
                ? get_op1()->fp().value_
                : (Float::ValueType)get_op1()->integer().value_;

        return L_FP(op1 - op0);
    }

    s32 op0_num, op0_div, op1_num, op1_div;
    if (!to_rational(get_op0(), op0_num, op0_div) ||
        !to_rational(get_op1(), op1_num, op1_div)) {
        return make_error("arguments must be numeric");
    }

    s32 result_num, result_div;
    sub_rationals(result_num, result_div, op1_num, op1_div, op0_num, op0_div);

    return make_ratio(result_num, result_div);
}


Value* builtin_multiply(int argc)
{
    if (argc == 0) {
        return make_integer(1); // Identity element
    }

    // Check for float contagion
    bool has_float = false;
    for (int i = 0; i < argc; ++i) {
        if (get_op(i)->type() == Value::Type::fp) {
            has_float = true;
            break;
        }
    }

    // Float path
    if (has_float) {
        Float::ValueType accum = 1.0f;
        for (int i = 0; i < argc; ++i) {
            if (get_op(i)->type() == Value::Type::fp) {
                accum *= get_op(i)->fp().value_;
            } else if (get_op(i)->type() == Value::Type::integer) {
                accum *= (Float::ValueType)get_op(i)->integer().value_;
            } else if (get_op(i)->type() == Value::Type::ratio) {
                auto& ratio = get_op(i)->ratio();
                s32 num = dcompr(ratio.numerator_)->integer().value_;
                Float::ValueType val = (Float::ValueType)num / ratio.divisor_;
                accum *= val;
            } else {
                return make_error("arguments must be numeric");
            }
        }
        return L_FP(accum);
    }

    // Rational path - accumulate as ratio
    s32 accum_num = 1, accum_div = 1;
    for (int i = 0; i < argc; ++i) {
        s32 op_num, op_div;
        if (!to_rational(get_op(i), op_num, op_div)) {
            return make_error("arguments must be numeric");
        }

        s32 result_num, result_div;
        mul_rationals(
            result_num, result_div, accum_num, accum_div, op_num, op_div);
        accum_num = result_num;
        accum_div = result_div;
    }

    return make_ratio(accum_num, accum_div);
}


Value* builtin_divide(int argc)
{
    // Handle float contagion first
    if (get_op0()->type() == Value::Type::fp ||
        get_op1()->type() == Value::Type::fp) {

        float divisor, dividend;

        // Convert divisor to float
        if (get_op0()->type() == Value::Type::fp) {
            divisor = get_op0()->fp().value_;
        } else {
            s32 num = 1, div = 1;
            to_rational(get_op0(), num, div);
            divisor = (float)num / (float)div; // Cast BOTH to float
        }

        // Convert dividend to float
        if (get_op1()->type() == Value::Type::fp) {
            dividend = get_op1()->fp().value_;
        } else {
            s32 num = 1, div = 1;
            to_rational(get_op1(), num, div);
            dividend = (float)num / (float)div; // Cast BOTH to float
        }

        if (divisor == 0.0f) {
            return make_error("division by zero");
        }
        return L_FP(dividend / divisor);
    }

    // Rational path
    s32 divisor_num, divisor_div, dividend_num, dividend_div;
    if (!to_rational(get_op0(), divisor_num, divisor_div) ||
        !to_rational(get_op1(), dividend_num, dividend_div)) {
        return make_error("arguments must be numeric");
    }
    if (divisor_num == 0) {
        return make_error("division by zero");
    }
    s32 result_num, result_div;
    div_rationals(result_num,
                  result_div,
                  dividend_num,
                  dividend_div,
                  divisor_num,
                  divisor_div);
    return make_ratio(result_num, result_div);
}


Value* builtin_mod(int argc)
{
    s32 divisor_num, divisor_div, dividend_num, dividend_div;
    if (!to_rational(get_op0(), divisor_num, divisor_div) ||
        !to_rational(get_op1(), dividend_num, dividend_div)) {
        return make_error("arguments must be numeric");
    }

    if (divisor_num == 0) {
        return make_error("division by zero");
    }

    s32 quot_num, quot_div;
    div_rationals(quot_num,
                  quot_div,
                  dividend_num,
                  dividend_div,
                  divisor_num,
                  divisor_div);

    s32 floor_val = quot_num / quot_div;
    if (quot_num < 0 && quot_num % quot_div != 0) {
        floor_val -= 1; // Adjust for negative
    }

    s32 mult_num, mult_div;
    mul_rationals(mult_num, mult_div, divisor_num, divisor_div, floor_val, 1);

    s32 result_num, result_div;
    sub_rationals(
        result_num, result_div, dividend_num, dividend_div, mult_num, mult_div);

    return make_ratio(result_num, result_div);
}


Value* builtin_rationalize(int argc)
{
    float f = get_op0()->fp().value_;

    if (f == 0.0f) {
        return make_integer(0);
    }

    // Handle sign
    bool negative = f < 0;
    if (negative)
        f = -f;

    // Continued fraction algorithm
    s32 n0 = 0, d0 = 1; // Previous convergent
    s32 n1 = 1, d1 = 0; // Current convergent

    float x = f;
    const int max_iterations = 10;
    const float epsilon = 0.000001f;

    for (int i = 0; i < max_iterations; i++) {
        s32 a = (s32)x; // Floor

        // Update convergents
        s32 n2 = a * n1 + n0;
        s32 d2 = a * d1 + d0;

        // Check if close enough
        float approx = (float)n2 / (float)d2;
        if (abs(approx - f) < epsilon) {
            if (negative)
                n2 = -n2;
            return make_ratio(n2, d2);
        }

        // Next iteration
        float frac = x - a;
        if (frac < epsilon)
            break;

        x = 1.0f / frac;
        n0 = n1;
        d0 = d1;
        n1 = n2;
        d1 = d2;
    }

    if (negative)
        n1 = -n1;
    return make_ratio(n1, d1);
}


Value* builtin_abs(int argc)
{
    L_EXPECT_RATIONAL(0);
    if (get_op0()->type() == Value::Type::integer) {
        auto old = L_LOAD_INT(0);
        if (old >= 0) {
            return get_op0();
        }
        return make_integer(abs(old));
    } else if (get_op0()->type() == Value::Type::ratio) {
        auto& old = get_op0()->ratio();
        auto old_num = dcompr(old.numerator_)->integer().value_;
        if (old_num >= 0) {
            return get_op0();
        }
        return make_ratio(abs(old_num), old.divisor_);
    }
    return L_NIL;
}


Value* builtin_int_to_bytes(int argc)
{
    L_EXPECT_OP(0, integer);
    ListBuilder result;
    host_s32 value;
    value.set(L_LOAD_INT(0));

    for (int i = 0; i < 4; ++i) {
        result.push_back(L_INT(((u8*)&value)[i]));
    }

    return result.result();
}


Value* builtin_bytes_to_int(int argc)
{
    if (not is_list(get_op0())) {
        return make_error("bytes-to-int expects a list of four values!");
    }

    host_s32 value;
    int i = 0;

    l_foreach(get_op0(),
              [&](Value* v) { ((u8*)&value)[i++] = v->integer().value_; });

    return L_INT(value.get());
}


Value* builtin_toint(int argc)
{
    if (get_op0()->type() == Value::Type::ratio) {
        auto& ratio = get_op0()->ratio();
        return make_integer(dcompr(ratio.numerator_)->integer().value_ /
                            ratio.divisor_);
    } else if (get_op0()->type() == Value::Type::string) {
        auto str = L_LOAD_STRING(0);

        int accum = 0;
        while (*str not_eq '\0') {
            accum = accum * 10 + (*str - '0');
            ++str;
        }
        return L_INT(accum);

    } else if (get_op0()->type() == Value::Type::fp) {
        return L_INT(L_LOAD_FP(0));
    } else if (get_op0()->type() == Value::Type::integer) {
        return get_op0();
    } else {
        return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                L_NIL);
    }
}


Value* builtin_tofloat(int argc)
{
    if (get_op0()->type() == Value::Type::string) {
        auto str = L_LOAD_STRING(0);
        return L_FP(atof(str));
    } else if (get_op0()->type() == Value::Type::fp) {
        return get_op0();
    } else if (get_op0()->type() == Value::Type::integer) {
        return L_FP(L_LOAD_INT(0));
    } else if (get_op0()->type() == Value::Type::ratio) {
        auto& ratio = get_op0()->ratio();
        s32 num = dcompr(ratio.numerator_)->integer().value_;
        return L_FP((float)num / (float)ratio.divisor_);
    } else {
        return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                L_NIL);
    }
}


Value* builtin_range(int argc)
{
    s32 start_num = 0, start_div = 1;
    s32 end_num = 0, end_div = 1;
    s32 incr_num = 1, incr_div = 1;

    if (argc == 1) {
        if (!to_rational(get_op0(), end_num, end_div)) {
            return make_error("arguments must be rational");
        }
    } else if (argc == 2) {
        if (!to_rational(get_op1(), start_num, start_div) ||
            !to_rational(get_op0(), end_num, end_div)) {
            return make_error("arguments must be rational");
        }
    } else if (argc == 3) {
        if (!to_rational(get_op(2), start_num, start_div) ||
            !to_rational(get_op1(), end_num, end_div) ||
            !to_rational(get_op0(), incr_num, incr_div)) {
            return make_error("arguments must be rational");
        }
    } else {
        return make_error("invalid argc");
    }

    if (incr_num == 0) {
        return get_nil();
    }

    ListBuilder lat;
    s32 current_num = start_num;
    s32 current_div = start_div;
    // Loop while current < end
    while (rational_less(current_num, current_div, end_num, end_div)) {
        lat.push_back(make_ratio(current_num, current_div));

        // current += incr
        s32 new_num, new_div;
        add_rationals(
            new_num, new_div, current_num, current_div, incr_num, incr_div);
        current_num = new_num;
        current_div = new_div;
        reduce_fraction(current_num, current_div);
    }

    return lat.result();
}


Value* builtin_incr(int argc)
{
    s32 num, div;
    if (!to_rational(get_op0(), num, div)) {
        return make_error("argument to incr must be int or ratio");
    }
    add_rationals(num, div, num, div, 1, 1);
    return make_ratio(num, div);
}


Value* builtin_decr(int argc)
{
    s32 num, div;
    if (!to_rational(get_op0(), num, div)) {
        return make_error("argument to decr must be int or ratio");
    }
    add_rationals(num, div, num, div, -1, 1);
    return make_ratio(num, div);
}


Value* builtin_hex(int argc)
{
    L_EXPECT_OP(0, integer);
    const char* hex = "0123456789abcdef";
    auto v = L_LOAD_INT(0);
    Buffer<char, 8> stack;
    while (v) {
        stack.push_back(hex[v & 0xf]);
        v >>= 4;
    }
    StringBuffer<10> result("0x");
    for (char c : reversed(stack)) {
        result.push_back(c);
    }
    return make_string(result.c_str());
}


Value* builtin_bit_and(int argc)
{
    L_EXPECT_OP(0, integer);
    L_EXPECT_OP(1, integer);
    return L_INT(L_LOAD_INT(1) & L_LOAD_INT(0));
}


Value* builtin_bit_or(int argc)
{
    int accum = 0;
    for (int i = 0; i < argc; ++i) {
        L_EXPECT_OP(i, integer);
        accum |= get_op(i)->integer().value_;
    }
    return make_integer(accum);
}


Value* builtin_bit_xor(int argc)
{
    L_EXPECT_OP(0, integer);
    L_EXPECT_OP(1, integer);
    return L_INT(L_LOAD_INT(1) ^ L_LOAD_INT(0));
}


Value* builtin_bit_not(int argc)
{
    L_EXPECT_OP(0, integer);
    return L_INT(~L_LOAD_INT(0));
}


Value* builtin_bit_shift_right(int argc)
{
    L_EXPECT_OP(0, integer);
    L_EXPECT_OP(1, integer);
    return L_INT(L_LOAD_INT(1) >> L_LOAD_INT(0));
}


Value* builtin_bit_shift_left(int argc)
{
    L_EXPECT_OP(0, integer);
    L_EXPECT_OP(1, integer);
    return L_INT(L_LOAD_INT(1) << L_LOAD_INT(0));
}


Value* builtin_wrap(int argc)
{
    L_EXPECT_OP(0, symbol);
    return wrap(get_op1(), get_op0());
}


Value* builtin_unwrap(int argc)
{
    L_EXPECT_OP(0, wrapped);
    if (get_op0()->hdr_.mode_bits_ == (u8)Wrapped::Variant::userdata) {
        return make_error("cannot unwrap userdata!");
    }
    return dcompr(get_op0()->wrapped().lisp_data_);
}


bool comp_less_than(Value* lhs, Value* rhs)
{
    // Check for float contagion
    if (lhs->type() == Value::Type::fp || rhs->type() == Value::Type::fp) {
        Float::ValueType lhs_val, rhs_val;

        // Convert lhs to float
        if (lhs->type() == Value::Type::fp) {
            lhs_val = lhs->fp().value_;
        } else {
            s32 num = 1, div = 1;
            to_rational(lhs, num, div);
            lhs_val = (Float::ValueType)num / div;
        }

        // Convert rhs to float
        if (rhs->type() == Value::Type::fp) {
            rhs_val = rhs->fp().value_;
        } else {
            s32 num = 1, div = 1;
            to_rational(rhs, num, div);
            rhs_val = (Float::ValueType)num / div;
        }

        return lhs_val < rhs_val;
    }

    // Rational comparison
    s32 lhs_num = 1, lhs_div = 1, rhs_num = 1, rhs_div = 1;
    to_rational(lhs, lhs_num, lhs_div);
    to_rational(rhs, rhs_num, rhs_div);

    return rational_less(lhs_num, lhs_div, rhs_num, rhs_div);
}


Value* builtin_comp_greater_than(int argc)
{
    // Check for float contagion
    if (get_op0()->type() == Value::Type::fp ||
        get_op1()->type() == Value::Type::fp) {

        Float::ValueType op0, op1;

        // Convert op0 to float
        if (get_op0()->type() == Value::Type::fp) {
            op0 = get_op0()->fp().value_;
        } else {
            s32 num, div;
            if (!to_rational(get_op0(), num, div)) {
                return make_error("arguments must be numeric");
            }
            op0 = (Float::ValueType)num / div;
        }

        // Convert op1 to float
        if (get_op1()->type() == Value::Type::fp) {
            op1 = get_op1()->fp().value_;
        } else {
            s32 num, div;
            if (!to_rational(get_op1(), num, div)) {
                return make_error("arguments must be numeric");
            }
            op1 = (Float::ValueType)num / div;
        }

        return make_integer(op1 > op0);
    }

    // Rational comparison
    s32 op0_num, op0_div, op1_num, op1_div;
    if (!to_rational(get_op0(), op0_num, op0_div) ||
        !to_rational(get_op1(), op1_num, op1_div)) {
        return make_error("arguments must be numeric");
    }

    return make_integer(rational_less(op0_num, op0_div, op1_num, op1_div));
}


Value* builtin_sort(int argc)
{
    L_EXPECT_OP(0, function);
    L_EXPECT_OP(1, cons);

    auto comp = get_op0();

    auto compare = [comp](Value* lhs, Value* rhs) {
        if (comp->hdr_.mode_bits_ == Function::ModeBits::cpp_function and
            comp->function().cpp_impl_ == builtin_comp_less_than) {
            return comp_less_than(lhs, rhs);
        } else {
            push_op(lhs);
            push_op(rhs);
            funcall(comp, 2);
            auto result = get_op0();
            pop_op(); // result
            return is_boolean_true(result);
        }
    };

    if (not is_list(get_op1())) {
        return make_error("sort parameter must be list!");
    }

    using TempBuffer = Buffer<Value*, 509>;
    auto buf = allocate_fast<TempBuffer>("sort-buffer");

    l_foreach(get_op1(), [&buf](Value* v) { buf->push_back(v); });

    std::sort(buf->begin(), buf->end(), compare);

    ListBuilder result;
    for (Value* v : *buf) {
        result.push_back(v);
    }

    return result.result();
}


Value* builtin_flatten(int argc)
{
    L_EXPECT_OP(0, cons);

    auto inp = get_op0();

    lisp::ListBuilder b;

    ::Function<6 * sizeof(void*), void(Value*)> flatten_impl([](Value*) {});
    flatten_impl = [&](Value* val) {
        if (is_list(val)) {
            l_foreach(val, flatten_impl);
        } else {
            b.push_back(val);
        }
    };

    l_foreach(inp, flatten_impl);

    return b.result();
}


Value* builtin_reverse(int argc)
{
    if (get_op0()->type() not_eq lisp::Value::Type::cons) {
        return L_NIL;
    }

    L_EXPECT_OP(0, cons);

    Value* result = get_nil();
    l_foreach(get_op0(), [&](Value* car) {
        push_op(result);
        result = make_cons(car, result);
        pop_op();
    });

    return result;
}


Value* builtin_filter(int argc)
{
    L_EXPECT_OP(0, cons);
    L_EXPECT_OP(1, function);

    auto fn = get_op1();
    Protected result(make_cons(L_NIL, L_NIL));
    auto prev = (Value*)result;
    auto current = (Value*)result;

    l_foreach(get_op0(), [&](Value* val) {
        push_op(val);
        funcall(fn, 1);
        auto funcall_result = get_op0();

        if (is_boolean_true(funcall_result)) {
            current->cons().set_car(val);
            auto next = make_cons(L_NIL, L_NIL);
            current->cons().set_cdr(next);
            prev = current;
            current = next;
        }
        pop_op(); // funcall result
    });

    if (current == result) {
        return L_NIL;
    }

    prev->cons().set_cdr(L_NIL);

    return (Value*)result;
}


Value* builtin_map(int argc)
{
    if (lisp::get_op(argc - 1)->type() not_eq Value::Type::function and
        lisp::get_op(argc - 1)->type() not_eq Value::Type::cons) {
        return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                L_NIL);
    }

    // I've never seen map used with so many input lists, but who knows,
    // someone might try to call this with more than six inputs...
    Buffer<Value*, 6> inp_lats;

    if (argc < static_cast<int>(inp_lats.size())) {
        return get_nil(); // TODO: return error
    }

    for (int i = 0; i < argc - 1; ++i) {
        L_EXPECT_OP(i, cons);
        inp_lats.push_back(get_op(i));
    }

    const auto len = length(inp_lats[0]);
    if (len == 0) {
        return get_nil();
    }
    for (auto& l : inp_lats) {
        if (length(l) not_eq len) {
            return get_nil(); // return error instead!
        }
    }

    auto fn = get_op(argc - 1);

    int index = 0;

    ListBuilder result;

    // Because the length function returned a non-zero value, we've
    // already succesfully scanned the list, so we don't need to do any
    // type checking.

    while (index < len) {

        for (auto& lat : reversed(inp_lats)) {
            push_op(lat->cons().car());
            lat = lat->cons().cdr();
        }
        funcall(fn, inp_lats.size());
        auto fc_result = get_op0();

        result.push_back(fc_result);
        pop_op();

        if (is_error(fc_result)) {
            return fc_result;
        }

        ++index;
    }

    return result.result();
}


Value* builtin_find(int argc)
{
    L_EXPECT_OP(0, cons);

    Optional<int> index;

    int i = 0;
    l_foreach(get_op0(), [&](Value* v) {
        if (index) {
            return;
        }
        if (is_equal(get_op1(), v)) {
            index = i;
        }
        ++i;
    });

    if (index) {
        return L_INT(*index);
    }

    return L_NIL;
}


Value* builtin_difference(int argc)
{
    ListBuilder list;

    auto find_difference = [&](Value* lat1, Value* lat2) {
        l_foreach(lat1, [&](Value* v1) {
            if (not contains(lat2, v1)) {
                list.push_back(v1);
            }
        });
    };
    find_difference(get_op0(), get_op1());
    find_difference(get_op1(), get_op0());

    return list.result();
}


Value* builtin_union(int argc)
{
    ListBuilder list;

    l_foreach(get_op0(), [&](Value* v) {
        if (not contains(list.result(), v) and contains(get_op1(), v)) {
            list.push_back(v);
        }
    });

    l_foreach(get_op1(), [&](Value* v) {
        if (not contains(list.result(), v) and contains(get_op0(), v)) {
            list.push_back(v);
        }
    });

    return list.result();
}


Value* builtin_length(int argc)
{
    if (get_op0()->type() == Value::Type::nil) {
        return make_integer(0);
    } else if (get_op0()->type() == Value::Type::string) {
        return make_integer(utf8::len(get_op0()->string().value()));
    }

    L_EXPECT_OP(0, cons);

    return make_integer(length(get_op0()));
}


Value* builtin_fill(int argc)
{
    L_EXPECT_OP(1, integer);

    auto result = make_list(get_op1()->integer().value_);
    for (int i = 0; i < get_op1()->integer().value_; ++i) {
        set_list(result, i, get_op0());
    }

    return result;
}


Value* builtin_get(int argc)
{
    if (get_op1()->type() == lisp::Value::Type::nil) {
        return L_NIL;
    }

    L_EXPECT_RATIONAL(0);

    const auto index = L_LOAD_INT(0);

    // if (get_op0()->type() == lisp::Value::Type::string) {
    //     auto str_data = L_LOAD_STRING(0);
    //     auto str_size = strlen(str_data);

    // }

    L_EXPECT_OP(1, cons);

    return get_list(get_op1(), index);
}


Value* builtin_logical_not(int argc)
{
    return make_integer(not is_boolean_true(get_op0()));
}


Value* builtin_comp_equal(int argc)
{
    return make_integer(is_equal(get_op0(), get_op1()));
}


Value* builtin_databuffer(int argc)
{
    return make_databuffer("lisp-databuffer");
}


Value* builtin_buffer_read(int argc)
{
    L_EXPECT_OP(0, integer);
    L_EXPECT_OP(1, integer);
    L_EXPECT_OP(2, databuffer);

    u16 len = L_LOAD_INT(0);
    u16 offset = L_LOAD_INT(1);

    if (offset + len >= SCRATCH_BUFFER_SIZE) {
        return make_boolean(false);
    }

    auto sbr = get_op(2)->databuffer().value();

    lisp::ListBuilder result;
    for (int i = 0; i < len; ++i) {
        result.push_back(L_INT((u8)sbr->data_[i + offset]));
    }

    return result.result();
}


Value* builtin_buffer_write(int argc)
{
    L_EXPECT_OP(0, cons);
    L_EXPECT_OP(1, integer);
    L_EXPECT_OP(2, databuffer);

    if (not is_list(get_op0())) {
        return make_boolean(false); // TODO: error
    }

    u16 len = length(get_op(0));
    u16 offset = L_LOAD_INT(1);

    if (offset + len >= SCRATCH_BUFFER_SIZE) {
        return make_boolean(false); // TODO: error
    }

    int i = offset;
    l_foreach(get_op0(), [&](Value* val) {
        u8 byte = val->integer().value_;
        get_op(2)->databuffer().value()->data_[i++] = byte;
    });

    return make_boolean(true);
}


Value* builtin_foreach(int argc)
{
    L_EXPECT_OP(1, function);
    L_EXPECT_OP(0, cons);

    auto fn = get_op1();

    l_foreach(get_op0(), [&](Value* val) {
        push_op(val);
        funcall(fn, 1);
        pop_op(); // result
    });

    return L_NIL;
}


Value* builtin_comp_less_than(int argc)
{
    // Validate arguments are numeric
    if (!is_numeric(get_op0()) || !is_numeric(get_op1())) {
        return make_error("arguments must be numeric");
    }

    return make_integer(comp_less_than(get_op1(), get_op0()));
}


Value* builtin_compile(int argc)
{
    L_EXPECT_OP(0, function);

    if (get_op0()->hdr_.mode_bits_ == Function::ModeBits::lisp_function) {
        auto input = get_op0();
        compile(dcompr(get_op0()->function().lisp_impl_.code_));
        auto ret = get_op0();
        if (ret->type() == Value::Type::function) {
            ret->function().sig_ = input->function().sig_;
        }
        pop_op();
        return ret;
    } else {
        return get_op0();
    }
}


Value* builtin_disassemble(int argc)
{
    L_EXPECT_OP(0, function);

    if (get_op0()->hdr_.mode_bits_ ==
        Function::ModeBits::lisp_bytecode_function) {

        Platform::RemoteConsole::Line out;

        u8 depth = 0;

        auto buffer = get_op0()->function().bytecode_impl_.databuffer();
        auto data = buffer->databuffer().value();

        Buffer<s32, 16> start_offsets;

        start_offsets.push_back(get_op0()
                                    ->function()
                                    .bytecode_impl_.bytecode_offset()
                                    ->integer()
                                    .value_);

        auto start_offset = [&] { return start_offsets.back(); };

        for (int i = start_offset(); i < SCRATCH_BUFFER_SIZE;) {

            const auto offset = to_string<10>(i - start_offset());
            if (offset.length() < 4) {
                for (u32 i = 0; i < 4 - offset.length(); ++i) {
                    out.push_back('0');
                }
            }

            out += offset;
            out += ": ";

            using namespace instruction;

            switch ((Opcode)(*data).data_[i]) {
            case Fatal::op():
                return get_nil();

            case LoadBuiltin::op(): {
                out += LoadBuiltin::name();
                out += "(";
                auto ptr = ((UnalignedPtr*)(data->data_ + i + 1))->get();
                out += nameof((Function::CPP_Impl)ptr);
                out += ":";
                out += stringify(
                    *(u8*)(data->data_ + i + 1 + sizeof(UnalignedPtr)));
                out += ")";
                i += sizeof(LoadBuiltin);
                break;
            }

            case load_var_nonlocal:
            case LoadVar::op():
                out += LoadVar::name();
                out += "(";
                out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
                out += ")";
                i += sizeof(LoadVar);
                break;

            case load_var_small_nonlocal:
            case LoadVarSmall::op(): {
                i += 1;
                out += "LOAD_VAR_SMALL(";
                StringBuffer<4> name;
                for (int j = 0; j < 4; ++j) {
                    name.push_back(*(data->data_ + i + j));
                }
                out += name.c_str();
                out += ")";
                i += 5;
                break;
            }

            case LoadLocalCached::op(): {
                i += 1;
                out += "LOAD_LOCAL_CACHED(";
                out += stringify((int)*(data->data_ + i));
                i += 1;
                out += ", ";
                out += stringify((int)*(data->data_ + i));
                i += 1;
                out += ")";
                i += *(data->data_ + i); // padding
                i += 1;
                break;
            }

            case LoadVarRelocatable::op():
                i += 1;
                out += "LOAD_VAR_RELOCATABLE(";
                out += to_string<32>(
                    ((HostInteger<s16>*)(data->data_ + i))->get());
                out += ")";
                i += 2;
                break;

            case PushSmallSymbol::op(): {
                i += 1;
                out += "PUSH_SMALL_SYMBOL(";
                StringBuffer<4> name;
                for (int j = 0; j < 4; ++j) {
                    name.push_back(*(data->data_ + i + j));
                }
                out += name.c_str();
                out += ")";
                i += 4;
                break;
            }

            case PushSymbol::op():
                out += "PUSH_SYMBOL(";
                out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
                out += ")";
                i += sizeof(PushSymbol);
                break;

            case PushSymbolRelocatable::op():
                i += 1;
                out += "PUSH_SYMBOL_RELOCATABLE(";
                out += to_string<32>(
                    ((HostInteger<s16>*)(data->data_ + i))->get());
                out += ")";
                i += 2;
                break;

            case PushString::op(): {
                i += 1;
                out += PushString::name();
                out += "(\"";
                u8 len = *(data->data_ + (i++));
                out += data->data_ + i;
                out += "\")";
                i += len;
                break;
            }

            case PushNil::op():
                out += "PUSH_NIL";
                i += 1;
                break;

            case Push0::op():
                i += 1;
                out += "PUSH_0";
                break;

            case Push1::op():
                i += 1;
                out += "PUSH_1";
                break;

            case Push2::op():
                i += 1;
                out += "PUSH_2";
                break;

            case PushRatio::op():
                i += 1;
                out += "PUSH_RATIO(";
                out += to_string<32>(
                    ((HostInteger<s32>*)(data->data_ + i))->get());
                out += "/";
                out += to_string<32>(
                    ((HostInteger<s32>*)(data->data_ + i + 4))->get());
                out += ")";
                i += 8;
                break;

            case PushInteger::op():
                i += 1;
                out += "PUSH_INTEGER(";
                out += to_string<32>(
                    ((HostInteger<s32>*)(data->data_ + i))->get());
                out += ")";
                i += 4;
                break;

            case PushSmallInteger::op():
                out += "PUSH_SMALL_INTEGER(";
                out += to_string<32>(*(data->data_ + i + 1));
                out += ")";
                i += 2;
                break;

            case PushFloat::op(): {
                out += "PUSH_FLOAT(";
                char buffer[32];
                auto f = ((PackedFloat*)(data->data_ + i + 1))->get();
                float_to_string(f, 32, buffer);
                out += buffer;
                out += ")";
                i += sizeof(PushFloat);
                break;
            }

            case JumpIfFalse::op():
                out += "JUMP_IF_FALSE(";
                out += to_string<32>(
                    ((HostInteger<u16>*)(data->data_ + i + 1))->get());
                out += ")";
                i += 3;
                break;

            case Jump::op():
                out += "JUMP(";
                out += to_string<32>(
                    ((HostInteger<u16>*)(data->data_ + i + 1))->get());
                out += ")";
                i += 3;
                break;

            case SmallJumpIfFalse::op():
                out += "SMALL_JUMP_IF_FALSE(";
                out += to_string<32>(*(data->data_ + i + 1));
                out += ")";
                i += 2;
                break;

            case SmallJump::op():
                out += "SMALL_JUMP(";
                out += to_string<32>(*(data->data_ + i + 1));
                out += ")";
                i += 2;
                break;

            case PushLambda::op(): {
                out += "PUSH_LAMBDA(";
                out += to_string<32>(
                    ((HostInteger<u16>*)(data->data_ + i + 1))->get());
                out += ")";
                i += 3;
                start_offsets.push_back(i);
                ++depth;
                break;
            }

            case PushThis::op():
                out += PushThis::name();
                i += sizeof(PushThis);
                break;

            case Arg::op():
                out += Arg::name();
                i += sizeof(Arg);
                break;

            case Arg0::op():
                out += Arg0::name();
                i += sizeof(Arg0);
                break;

            case Arg1::op():
                out += Arg1::name();
                i += sizeof(Arg1);
                break;

            case Arg2::op():
                out += Arg2::name();
                i += sizeof(Arg2);
                break;

            case TailCall::op():
                out += TailCall::name();
                out += "(";
                out += to_string<32>(*(data->data_ + i + 1));
                out += ")";
                i += 2;
                break;

            case TailCall1::op():
                out += TailCall1::name();
                ++i;
                break;

            case TailCall2::op():
                out += TailCall2::name();
                ++i;
                break;

            case TailCall3::op():
                out += TailCall3::name();
                ++i;
                break;

            case Funcall::op():
                out += "FUNCALL(";
                out += to_string<32>(*(data->data_ + i + 1));
                out += ")";
                i += 2;
                break;

            case PushList::op():
                out += "PUSH_LIST(";
                out += to_string<32>(*(data->data_ + i + 1));
                out += ")";
                i += 2;
                break;

            case Funcall1::op():
                out += "FUNCALL_1";
                i += 1;
                break;

            case Funcall2::op():
                out += "FUNCALL_2";
                i += 1;
                break;

            case Funcall3::op():
                out += "FUNCALL_3";
                i += 1;
                break;

            case Pop::op():
                out += "POP";
                i += 1;
                break;

            case MakePair::op():
                out += "MAKE_PAIR";
                i += 1;
                break;

            case Not::op():
                out += Not::name();
                i += sizeof(Not);
                break;

            case First::op():
                out += First::name();
                i += sizeof(First);
                break;

            case Rest::op():
                out += Rest::name();
                i += sizeof(Rest);
                break;

            case Dup::op():
                out += Dup::name();
                i += 1;
                break;

            case EarlyRet::op():
                out += EarlyRet::name();
                i += sizeof(EarlyRet);
                break;

            case LexicalDef::op():
                out += LexicalDef::name();
                out += "(";
                out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
                out += ")";
                i += sizeof(LexicalDef);
                break;

            case LexicalDefSmallFromArg0::op(): {
                out += LexicalDefSmallFromArg0::name();
                out += "(";
                i += 1;
                StringBuffer<4> name;
                for (int j = 0; j < 4; ++j) {
                    name.push_back(*(data->data_ + i + j));
                }
                out += name.c_str();
                out += ")";
                i += 4;
                break;
            }

            case LexicalDefSmallFromArg1::op(): {
                out += LexicalDefSmallFromArg1::name();
                out += "(";
                i += 1;
                StringBuffer<4> name;
                for (int j = 0; j < 4; ++j) {
                    name.push_back(*(data->data_ + i + j));
                }
                out += name.c_str();
                out += ")";
                i += 4;
                break;
            }

            case LexicalDefSmallFromArg2::op(): {
                out += LexicalDefSmallFromArg2::name();
                out += "(";
                i += 1;
                StringBuffer<4> name;
                for (int j = 0; j < 4; ++j) {
                    name.push_back(*(data->data_ + i + j));
                }
                out += name.c_str();
                out += ")";
                i += 4;
                break;
            }

            case LexicalDefSmall::op(): {
                out += LexicalDefSmall::name();
                out += "(";
                i += 1;
                StringBuffer<4> name;
                for (int j = 0; j < 4; ++j) {
                    name.push_back(*(data->data_ + i + j));
                }
                out += name.c_str();
                out += ")";
                i += 4;
                break;
            }

            case LexicalDefRelocatable::op():
                out += LexicalDefRelocatable::name();
                out += "(";
                out += to_string<32>(
                    ((HostInteger<s16>*)(data->data_ + i + 1))->get());
                out += ")";
                i += sizeof(LexicalDefRelocatable);
                break;

            case LexicalFramePush::op():
                out += LexicalFramePush::name();
                i += sizeof(LexicalFramePush);
                break;

            case LexicalFramePop::op():
                out += LexicalFramePop::name();
                i += sizeof(LexicalFramePop);
                break;

            case LexicalVarLoad::op():
                out += LexicalVarLoad::name();
                i += sizeof(LexicalVarLoad);
                break;

            case Await::op(): {
                out += Await::name();
                i += sizeof(Await);
                break;
            }

            case Ret::op(): {
                if (depth == 0) {
                    out += "RET\r\n";
                    auto pfrm = &PLATFORM;
                    if (pfrm->remote_console().printline(out.c_str(), "")) {
                        ((Platform*)pfrm)->sleep(80);
                    } else {
                        info(out.c_str());
                    }
                    return get_nil();
                } else {
                    --depth;
                    out += "RET";
                    i += 1;
                }
                start_offsets.pop_back();
                break;
            }

            default:
                PLATFORM.remote_console().printline(out.c_str(), "");
                PLATFORM.sleep(80);
                return get_nil();
            }
            out += "\r\n";
        }
        return get_nil();
    } else if (get_op0()->hdr_.mode_bits_ ==
               Function::ModeBits::lisp_function) {

        auto expression_list = dcompr(get_op0()->function().lisp_impl_.code_);

        Protected sym(make_symbol("fn"));

        return make_cons(sym, expression_list);

    } else {
        return get_nil();
    }
}


Value* builtin_nameof(int argc)
{
    if (auto name = nameof(get_op0())) {
        return make_symbol(name);
    }
    return L_NIL;
}


Value* builtin_apropos(int argc)
{
    L_EXPECT_OP(0, string);

    Vector<const char*> results;
    apropos(L_LOAD_STRING(0), results);

    ListBuilder list;
    for (auto& r : results) {
        list.push_back(make_string(r));
    }

    return list.result();
}


Value* builtin_is_odd(int argc)
{
    if (get_op0()->type() == Value::Type::integer) {
        return make_boolean(L_LOAD_INT(0) % 2);
    }
    return make_boolean(false);
}


Value* builtin_is_wrapped(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::wrapped);
}


Value* builtin_is_string(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::string);
}


Value* builtin_is_databuffer(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::databuffer);
}


Value* builtin_is_symbol(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::symbol);
}


Value* builtin_is_error(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::error);
}


Value* builtin_is_lambda(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::function);
}


Value* builtin_is_pair(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::cons);
}


Value* builtin_is_float(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::fp);
}


Value* builtin_is_int(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::integer);
}


Value* builtin_is_ratio(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::ratio);
}


Value* builtin_is_nil(int argc)
{
    return make_boolean(get_op0()->type() == Value::Type::nil);
}


Value* builtin_is_list(int argc)
{
    return make_boolean(is_list(get_op0()));
}


Value* builtin_is_boolean(int argc)
{
    return make_boolean((get_op0()->type() == Value::Type::integer and
                         (get_op0()->integer().value_ == 1 or
                          get_op0()->integer().value_ == 0)) or
                        get_op0()->type() == Value::Type::nil);
}


Value* builtin_type(int argc)
{
    if (get_op0()->type() == Value::Type::wrapped) {
        return dcompr(get_op0()->wrapped().type_sym_);
    }
    return make_symbol(type_to_string(get_op0()->type()));
}


Value* builtin_profile(int argc)
{
    L_EXPECT_OP(0, function);
    auto start = PLATFORM.delta_clock().sample();
    funcall(get_op0(), 0);
    auto stop = PLATFORM.delta_clock().sample();
    pop_op();
    return L_INT(stop - start);
}


Value* builtin_error(int argc)
{
    L_EXPECT_OP(0, string);
    return make_error(Error::Code::custom, get_op0());
}


Value* builtin_error_info(int argc)
{
    if (get_op0()->type() == Value::Type::error) {
        return dcompr(get_op0()->error().context_);
    }
    return L_NIL;
}


Value* builtin_breakpoint_reg(int argc)
{
    lisp::debug::register_symbol_breakpoint(lisp::get_op0());
    return L_NIL;
}


Value* builtin_breakpoint_unreg(int argc)
{
    lisp::debug::delete_symbol_breakpoint(lisp::get_op0());
    return L_NIL;
}


Value* builtin_watchpoint_reg(int argc)
{
    lisp::debug::register_symbol_watchpoint(lisp::get_op0());
    return L_NIL;
}


Value* builtin_watchpoint_unreg(int argc)
{
    lisp::debug::delete_symbol_watchpoint(lisp::get_op0());
    return L_NIL;
}


} // namespace lisp
