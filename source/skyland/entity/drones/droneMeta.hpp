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

#include "drone.hpp"
#include "script/lisp.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/coins.hpp"



namespace skyland
{



struct DroneMeta
{

    static const DroneMeta* load(const char* name);


    static int index(const char* name);


    struct Box
    {
        virtual ~Box()
        {
        }

        virtual std::optional<SharedEntityRef<Drone>>
        create(Island* parent,
               Island* destination,
               const RoomCoord& grid_pos) const = 0;


        virtual const char* name() const = 0;


        virtual u16 icon() const = 0;


        virtual u16 unsel_icon() const = 0;


        virtual void configure(/* TODO... */)
        {
        }


        virtual Coins cost() const = 0;
    };


    template <typename T> struct BoxImpl : public Box
    {
        BoxImpl()
        {
        }


        const char* name() const
        {
            return T::get_name();
        }


        u16 icon() const override
        {
            return T::icon();
        }


        u16 unsel_icon() const override
        {
            return T::unsel_icon();
        }


        Coins cost() const override
        {
            return T::cost();
        }


        std::optional<SharedEntityRef<Drone>>
        create(Island* parent,
               Island* destination,
               const RoomCoord& grid_pos) const override
        {
            return alloc_shared_entity<T, Drone>(parent, destination, grid_pos);
        }
    };

    static constexpr const auto align = alignof(void*);
    alignas(align) u8 buffer_[8];


    template <typename T> void init()
    {
        static_assert(sizeof buffer_ >= sizeof(BoxImpl<T>));
        static_assert(align >= alignof(BoxImpl<T>));

        new (buffer_) BoxImpl<T>();
    }

    DroneMeta()
    {
    }

    DroneMeta(const DroneMeta&) = delete;

    Box* operator->()
    {
        return reinterpret_cast<Box*>(buffer_);
    }

    const Box* operator->() const
    {
        return reinterpret_cast<const Box*>(buffer_);
    }

    ~DroneMeta()
    {
        reinterpret_cast<Box*>(buffer_)->~Box();
    }
};



template <typename... Drones> struct DroneMetatable
{
public:
    template <size_t i, typename First, typename... Rest> void init()
    {
        table_[i].template init<First>();

        if constexpr (sizeof...(Rest) > 0) {
            init<i + 1, Rest...>();
        }
    }

    DroneMetatable()
    {
        init<0, Drones...>();
    }

    constexpr int size() const
    {
        return sizeof...(Drones);
    }

    DroneMeta table_[sizeof...(Drones)];
};



std::pair<const DroneMeta*, int> drone_metatable();



} // namespace skyland
