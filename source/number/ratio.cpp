////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "ratio.hpp"



void reduce_fraction(s32& num, s32& div)
{
    if (num == 0) {
        div = 1;
        return;
    }

    if (div < 0) {
        num = -num;
        div = -div;
    }

    s32 a = num < 0 ? -num : num; // abs(num)
    s32 b = div;

    while (b != 0) {
        s32 temp = b;
        b = a % b;
        a = temp;
    }
    num /= a;
    div /= a;
}



void div_rationals(s32& result_num,
                   s32& result_div,
                   s32 a_num,
                   s32 a_div,
                   s32 b_num,
                   s32 b_div)
{
    // (a/b) / (c/d) = (a/b) * (d/c) = (a*d) / (b*c)
    result_num = a_num * b_div;
    result_div = a_div * b_num;

    // Handle sign (denominator must be positive)
    if (result_div < 0) {
        result_num = -result_num;
        result_div = -result_div;
    }
}



void sub_rationals(s32& result_num,
                   s32& result_div,
                   s32 a_num,
                   s32 a_div,
                   s32 b_num,
                   s32 b_div)
{
    // a/b - c/d = (a*d - c*b) / (b*d)
    result_num = a_num * b_div - b_num * a_div;
    result_div = a_div * b_div;
}



void mul_rationals(s32& result_num,
                   s32& result_div,
                   s32 a_num,
                   s32 a_div,
                   s32 b_num,
                   s32 b_div)
{
    // (a/b) * (c/d) = (a*c) / (b*d)
    result_num = a_num * b_num;
    result_div = a_div * b_div;
}



void add_rationals(s32& result_num,
                   s32& result_div,
                   s32 a_num,
                   s32 a_div,
                   s32 b_num,
                   s32 b_div)
{
    // Formula: a/b + c/d = (a*d + b*c) / (b*d)
    result_num = a_num * b_div + b_num * a_div;
    result_div = a_div * b_div;
}



Ratio operator+(const Ratio& lhs, const Ratio& rhs)
{
    Ratio result;
    add_rationals(
        result.num_, result.div_, lhs.num_, lhs.div_, rhs.num_, rhs.div_);
    reduce_fraction(result.num_, result.div_);
    return result;
}



Ratio operator-(const Ratio& lhs, const Ratio& rhs)
{
    Ratio result;
    sub_rationals(
        result.num_, result.div_, lhs.num_, lhs.div_, rhs.num_, rhs.div_);
    reduce_fraction(result.num_, result.div_);
    return result;
}
