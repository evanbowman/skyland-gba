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

#include "value.hpp"


namespace lisp
{


// Protected objects will not be collected until the Protected wrapper goes out
// of scope.
class ProtectedBase
{
public:
    ProtectedBase();

    ProtectedBase(const ProtectedBase&) = delete;

    ProtectedBase(ProtectedBase&&) = delete;

    virtual ~ProtectedBase();

    virtual void gc_mark() = 0;

    ProtectedBase* next() const
    {
        return next_;
    }

    ProtectedBase* prev() const
    {
        return prev_;
    }

protected:
    ProtectedBase* prev_;
    ProtectedBase* next_;
};


class Protected : public ProtectedBase
{
public:
    Protected(Value* val) : val_(val)
    {
    }

    void gc_mark() override;

    Protected& operator=(Value* val)
    {
        val_ = val;
        return *this;
    }

    void set(Value* val)
    {
        val_ = val;
    }

    operator Value*()
    {
        return val_;
    }

    Value* get() const
    {
        return val_;
    }

    Value* operator->()
    {
        return val_;
    }

protected:
    Value* val_;
};


} // namespace lisp
