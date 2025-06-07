////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
