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

#include <array>
#include <ciso646>
#include <stdint.h>


// A fixed-space version of std::function, does not allocate.


template <std::size_t storage, typename T> class Function
{
};


template <std::size_t storage, typename R, typename... Args>
class Function<storage, R(Args...)>
{
public:
    void* data()
    {
        return internal_storage_.data();
    }


    const void* data() const
    {
        return internal_storage_.data();
    }


    template <typename Functor>
    Function(Functor f)
        : invoke_policy_(reinterpret_cast<InvokePolicy>(invokeImpl<Functor>)),
          construct_policy_(
              reinterpret_cast<ConstructPolicy>(constructImpl<Functor>)),
          move_policy_(reinterpret_cast<MovePolicy>(moveImpl<Functor>)),
          destroy_policy_(reinterpret_cast<DestroyPolicy>(destroyImpl<Functor>))
    {
        static_assert(storage >= sizeof(Functor));
        static_assert(alignof(Functor) <= alignof(Functor),
                      "Function uses a hard-coded maximum alignment of"
                      " eight bytes. You can increase it to 16 or higher if"
                      " it\'s really necessary...");
        construct_policy_(data(), reinterpret_cast<void*>(&f));
    }


    Function(Function const& rhs)
        : invoke_policy_(rhs.invoke_policy_),
          construct_policy_(rhs.construct_policy_),
          move_policy_(rhs.move_policy_), destroy_policy_(rhs.destroy_policy_)
    {
        if (invoke_policy_) {
            construct_policy_(data(), rhs.data());
        }
    }


    Function& operator=(const Function& rhs)
    {
        if (destroy_policy_) {
            destroy_policy_(data());
        }

        invoke_policy_ = rhs.invoke_policy_;
        construct_policy_ = rhs.construct_policy_;
        move_policy_ = rhs.move_policy_;
        destroy_policy_ = rhs.destroy_policy_;

        if (invoke_policy_) {
            construct_policy_(data(), rhs.data());
        }

        return *this;
    }


    Function(Function&& rhs)
        : invoke_policy_(rhs.invoke_policy_),
          construct_policy_(rhs.construct_policy_),
          move_policy_(rhs.move_policy_), destroy_policy_(rhs.destroy_policy_)
    {
        rhs.invoke_policy_ = nullptr;
        rhs.construct_policy_ = nullptr;
        rhs.destroy_policy_ = nullptr;
        if (invoke_policy_) {
            move_policy_(data(), rhs.data());
        }
    }


    ~Function()
    {
        if (destroy_policy_) {
            destroy_policy_(data());
        }
    }


    R operator()(Args&&... args)
    {
        return invoke_policy_(data(), std::forward<Args>(args)...);
    }


private:
    typedef R (*InvokePolicy)(void*, Args&&...);
    typedef void (*ConstructPolicy)(void*, const void*);
    typedef void (*MovePolicy)(void*, void*);
    typedef void (*DestroyPolicy)(void*);


    template <typename Functor> static R invokeImpl(Functor* fn, Args&&... args)
    {
        return (*fn)(std::forward<Args>(args)...);
    }


    template <typename Functor>
    static void constructImpl(Functor* construct_dst,
                              const Functor* construct_src)
    {
        new (construct_dst) Functor(*construct_src);
    }


    template <typename Functor>
    static void moveImpl(Functor* move_dst, Functor* move_src)
    {
        new (move_dst) Functor(std::move(*move_src));
    }


    template <typename Functor> static void destroyImpl(Functor* f)
    {
        f->~Functor();
    }


    InvokePolicy invoke_policy_;
    ConstructPolicy construct_policy_;
    MovePolicy move_policy_;
    DestroyPolicy destroy_policy_;
    // TODO: 16 Or higher alignment is a somewhat unusual edge case, but the
    // code _should_ be updated to handle it.
    alignas(8) std::array<uint8_t, storage> internal_storage_;
};
