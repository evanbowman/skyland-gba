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
        return head_;
    }

private:
    Protected head_;
    Value* tail_ = nullptr;
    int length_ = 0;
};


} // namespace lisp
