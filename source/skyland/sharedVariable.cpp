////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "sharedVariable.hpp"
#include "string.hpp"



namespace skyland
{



static SharedVariable* __shared_variables = nullptr;



SharedVariable::SharedVariable(const char* name) : name_(name), value_(0)
{
    next_ = __shared_variables;
    __shared_variables = this;
}



SharedVariable::SharedVariable(const char* name, int initial)
    : name_(name), value_(initial)
{
    next_ = __shared_variables;
    __shared_variables = this;
}



SharedVariable* SharedVariable::load(const char* name)
{
    auto list = __shared_variables;

    while (list) {
        if (str_cmp(list->name_, name) == 0) {
            return list;
        }
        list = list->next_;
    }

    return nullptr;
}



SharedVariable::~SharedVariable()
{
    auto list = __shared_variables;
    SharedVariable* prev = nullptr;

    while (list) {
        const bool found_self = list == this;
        if (found_self) {
            if (prev) {
                // Unlink ourself from the list.
                prev->next_ = next_;
            } else {
                // We're the first element of the list!
                __shared_variables = next_;
            }
            return;
        }

        prev = list;
        list = list->next_;
    }
}



} // namespace skyland
