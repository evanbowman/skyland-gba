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
