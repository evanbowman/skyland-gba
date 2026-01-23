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

#include "int.h"



void div_rationals(s32& result_num, s32& result_div,
                   s32 a_num, s32 a_div,
                   s32 b_num, s32 b_div);


void sub_rationals(s32& result_num, s32& result_div,
                   s32 a_num, s32 a_div,
                   s32 b_num, s32 b_div);


void mul_rationals(s32& result_num, s32& result_div,
                   s32 a_num, s32 a_div,
                   s32 b_num, s32 b_div);


void add_rationals(s32& result_num, s32& result_div,
                   s32 a_num, s32 a_div,
                   s32 b_num, s32 b_div);


void reduce_fraction(s32& num, s32& div);
