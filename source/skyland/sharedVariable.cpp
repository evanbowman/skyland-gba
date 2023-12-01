////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
