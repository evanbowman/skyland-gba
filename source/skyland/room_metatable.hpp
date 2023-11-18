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


#include "bitvector.hpp"
#include "coins.hpp"
#include "island.hpp"
#include "metaclassIndex.hpp"
#include "room.hpp"
#include "rooms/pluginRoom.hpp"
#include "script/lisp.hpp"
#include "systemString.hpp"



namespace skyland
{



// Why metaclasses? We need to be able to request info about a room before
// instantiating one, so mostly an organizational choice.
struct RoomMeta
{

    struct Info
    {
        virtual ~Info()
        {
        }

        virtual void
        construct(void* address, Island* parent, const RoomCoord& position) = 0;

        // DO NOT CALL THIS FUNCTION UNLESS YOU REALLY KNOW WHAT YOU'RE DOING. I
        // added this interface as a hack to make multiboot stuff easier to
        // write.

        virtual void create(App&,
                            Island*,
                            const RoomCoord&,
                            bool do_repaint = true) const = 0;
        virtual RoomPtr<Room> create(Island*, const RoomCoord&) const = 0;
        virtual const char* name() const = 0;
        virtual SystemStringBuffer ui_name() const = 0;
        virtual Vec2<u8> size() const = 0;
        virtual Coins cost() const = 0;
        virtual Float atp_value() const = 0;
        virtual Power consumes_power() const = 0;
        virtual RoomProperties::Bitmask properties() const = 0;
        virtual Room::Icon icon() const = 0;
        virtual Room::Icon unsel_icon() const = 0;
        virtual Health full_health() const = 0;
        virtual Room::Category category() const = 0;
        virtual void format_description(StringBuffer<512>& buffer) const = 0;
        virtual Room::WeaponOrientation weapon_orientation() const = 0;

        virtual void configure(Health health, Coins cost, Power power)
        {
        }
    };



    static constexpr int align = 8;
    static constexpr int max_size = 10 * sizeof(void*);

    alignas(align) u8 buffer_[max_size];


    template <typename T> void init();


    void init_plugin();


    RoomMeta()
    {
    }

    RoomMeta(const RoomMeta&) = delete;

    Info* operator->()
    {
        return reinterpret_cast<Info*>(buffer_);
    }

    const Info* operator->() const
    {
        return reinterpret_cast<const Info*>(buffer_);
    }

    Info* box()
    {
        return reinterpret_cast<Info*>(buffer_);
    }

    ~RoomMeta()
    {
        reinterpret_cast<Info*>(buffer_)->~Info();
    }
};



std::pair<RoomMeta*, int> room_metatable();



MetaclassIndex plugin_rooms_begin();



bool is_enabled(MetaclassIndex index);



void set_enabled(MetaclassIndex index, bool enabled);



void plugin_rooms_unregister();



bool plugin_room_register(lisp::Value* config);



MetaclassIndex metaclass_index(const char* name);



RoomMeta* load_metaclass(const char* name);
RoomMeta& require_metaclass(const char* name);
RoomMeta* load_metaclass(MetaclassIndex index);


// Some cached metaclass pointers, for convenience.
extern const RoomMeta* forcefield_mt;
extern const RoomMeta* forcefield2_mt;
extern const RoomMeta* cannon_mt;
extern const RoomMeta* missile_silo_mt;
extern const RoomMeta* ion_cannon_mt;
extern const RoomMeta* bulkhead_mt;
extern const RoomMeta* drone_bay_mt;


inline bool is_forcefield(RoomMeta* m)
{
    return m == forcefield_mt or m == forcefield2_mt;
}



void room_set_hidden(MetaclassIndex idx, bool hidden);
bool room_hidden(MetaclassIndex);



void load_hidden_rooms();
void store_hidden_rooms();



} // namespace skyland
