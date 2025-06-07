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

#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland
{



static const int construction_zone_min_y = 4;



class ConstructionScene : public ActiveWorldScene
{
public:
    ConstructionScene(bool near = true)
        : data_(allocate_dynamic<Data>("construction-data")), near_(near)
    {
    }


    ConstructionScene(int selector)
        : selector_(selector),
          data_(allocate_dynamic<Data>("construction-data"))
    {
    }


    ScenePtr update(Time delta) override;


    void display() override;


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    bool camera_update_check_key() override;


    ConstructionScene* cast_construction_scene() override
    {
        return this;
    }


    void open_prompt_at(MetaclassIndex mti)
    {
        jump_to_selection_ = mti;
    }


private:
    enum class State {
        select_loc,
        choose_building,
        add_terrain,
        insufficient_funds,
    };


    bool site_has_space(MetaclassIndex m);


    Island* island();


    bool collect_available_buildings();


    void find_construction_sites();


    void msg(const char* text);


    void show_current_building_text();


    using Coord = Vec2<s8>;


    u32 selector_ = 0;

    Optional<Text> text_;
    Optional<Text> category_label_;

    struct Data
    {
        Buffer<Coord, 48> construction_sites_;
        Buffer<MetaclassIndex, 100> available_buildings_;
        Optional<MetaclassIndex> last_constructed_building_;
    };

    DynamicMemory<Data> data_;

    bool show_category_ = true;
    Room::Category last_category_ = Room::Category::count;

    State state_ = State::select_loc;

    int building_selector_ = 0;

    int touchscroll_ = 0;
    int last_touch_x_ = 0;

    Time flicker_timer_ = 0;

    BlockChecksum checksum_;

    bool flicker_on_ = false;

    bool near_;
    u8 stack_ = 0;

    Optional<u8> jump_to_selection_;

    static bool constrain_;
};



void make_construction_effect(Vec2<Fixnum> pos);



} // namespace skyland
