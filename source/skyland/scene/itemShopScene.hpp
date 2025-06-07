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

#include "skyland/room_metatable.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ItemShopScene : public WorldScene
{
public:
    ItemShopScene();


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


private:
    struct ShopItem
    {
        MetaclassIndex mt_;
        u16 price_;
        u16 qty_;
    };

    using ItemsBuffer = Buffer<ShopItem, 4>;
    DynamicMemory<ItemsBuffer> items_;

    Time timer_ = 0;

    Vec2<u8> cursor_;

    void describe_selection();

    u32 item_slot(int x, int y);

    enum class State : u8 {
        fade_in,
        animate_box,
        ready,
    } state_ = State::fade_in;
};



} // namespace skyland
