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

#include "skyland/room_metatable.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ItemShopScene : public WorldScene
{
public:
    ItemShopScene();


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    struct ShopItem
    {
        MetaclassIndex mt_;
        u16 price_;
        u16 qty_;
    };

    using ItemsBuffer = Buffer<ShopItem, 4>;
    DynamicMemory<ItemsBuffer> items_;

    Microseconds timer_ = 0;

    Vec2<u8> cursor_;

    void describe_selection(Platform&);

    u32 item_slot(int x, int y);

    enum class State : u8 {
        fade_in,
        animate_box,
        ready,
    } state_ = State::fade_in;
};



} // namespace skyland
