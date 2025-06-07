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


namespace lisp
{


struct Module
{
    struct Header
    {
        host_u16 symbol_count_;
        host_u16 bytecode_length_;
    } header_;

    // char symbol_data_[];
    // char bytecode_[];
};


} // namespace lisp
