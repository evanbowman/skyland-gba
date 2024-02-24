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



#include "room_metatable.hpp"



namespace skyland
{



// A metatable entry backed by a lisp datastructure, allowing users to
// define their own rooms via scripts.
struct RoomPluginInfo : public RoomMeta::Info
{
    RoomMeta* mt_;
    mutable Optional<lisp::Protected> info_;

    s16 health_ = 10;
    s16 cost_ = 10;
    s16 power_ = 10;


    RoomPluginInfo(RoomMeta* mt) : mt_(mt)
    {
    }


    void
    construct(void* address, Island* parent, const RoomCoord& position) override
    {
        static_assert(sizeof(PluginRoom) <= room_pool::max_room_size);
        static_assert(alignof(PluginRoom) <= room_pool::alignment);

        new (address) PluginRoom(parent, position, mt_);
    }


    void create(Island* parent,
                const RoomCoord& position,
                bool do_repaint) const override
    {
        parent->add_room<PluginRoom>(position, do_repaint, mt_);
    }


    struct FieldTag
    {
        enum Tag {
            name,
            size,
            graphics_list,
            update_frequency,
            update,
        };
    };


    Room::WeaponOrientation weapon_orientation() const override
    {
        return Room::WeaponOrientation::none;
    }


    template <FieldTag::Tag info, typename T> T& fetch_info() const
    {
        if (info_) {
            return lisp::get_list(*info_, info)->expect<T>();
        }

        Platform::fatal("plugin room info unassigned");
    }


    const char* name() const override
    {
        return fetch_info<FieldTag::name, lisp::Symbol>().name();
    }


    SystemStringBuffer ui_name() const override
    {
        auto ret = allocate_dynamic<StringBuffer<1900>>("locale-string");
        *ret += name();
        return ret;
    }


    Vec2<u8> size() const override
    {
        auto& pair = fetch_info<FieldTag::size, lisp::Cons>();
        return {(u8)pair.car()->expect<lisp::Integer>().value_,
                (u8)pair.cdr()->expect<lisp::Integer>().value_};
    }


    Coins cost() const override
    {
        return cost_;
    }


    ATP atp_value() const override
    {
        // FIXME!
        return 2.0_atp;
    }


    Power consumes_power() const override
    {
        return power_;
    }


    RoomProperties::Bitmask properties() const override
    {
        return RoomProperties::plugin | RoomProperties::disallow_chimney |
               RoomProperties::roof_hidden | RoomProperties::locked_by_default;
    }


    Room::Category category() const override
    {
        return Room::Category::misc; // TODO...
    }


    void format_description(StringBuffer<512>&) const override
    {
        Platform::fatal("attempt to fetch desciption for a plugin room.");
    }


    Room::Icon icon() const override
    {
        return PluginRoom::icon();
    }


    Room::Icon unsel_icon() const override
    {
        return PluginRoom::unsel_icon();
    }


    Health full_health() const override
    {
        return health_;
    }
};



} // namespace skyland
