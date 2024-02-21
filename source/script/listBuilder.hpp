////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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


#pragma once


#include "lisp.hpp"


namespace lisp
{


class ListBuilder
{
public:
    ListBuilder() : head_(get_nil())
    {
    }

    void push_front(Value* val)
    {
        ++length_;

        Protected protected_val(val);

        auto next = make_cons(val, head_);
        next->cons().is_definitely_list_ = true;

        if (tail_ == nullptr) {
            tail_ = next;
        }

        head_.set(next);
    }

    void push_back(Value* val)
    {
        if (tail_ == nullptr) {
            push_front(val);
        } else {
            ++length_;
            Protected protected_val(val);
            if (tail_->type() == Value::Type::cons) {
                auto new_tail = make_cons(val, L_NIL);
                new_tail->cons().is_definitely_list_ = true;
                tail_->cons().__set_cdr(new_tail);
                tail_ = new_tail;
            }
        }
    }

    Value* result()
    {
        if (length_ < 127) {
            head_->cons().cached_length_ = length_;
        }
        return head_;
    }

private:
    Protected head_;
    Value* tail_ = nullptr;
    int length_ = 0;
};


} // namespace lisp
