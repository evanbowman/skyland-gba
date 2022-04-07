////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "lisp.hpp"


namespace lisp {


class ListBuilder {
public:
    ListBuilder() : head_(get_nil())
    {
    }

    void push_front(Value* val)
    {
        Protected protected_val(val);

        auto next = make_cons(val, head_);

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
            Protected protected_val(val);
            if (tail_->type() == Value::Type::cons) {
                auto new_tail = make_cons(val, L_NIL);
                tail_->cons().set_cdr(new_tail);
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
};


} // namespace lisp
