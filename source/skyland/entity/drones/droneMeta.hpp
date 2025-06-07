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

        virtual Optional<SharedEntityRef<Drone>>
        create(Island* parent,
               Island* destination,
               const RoomCoord& grid_pos) const = 0;


        virtual bool spawn_near() const = 0;


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


        bool spawn_near() const override
        {
            return T::spawn_near();
        }


        const char* name() const override
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


        Optional<SharedEntityRef<Drone>>
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
