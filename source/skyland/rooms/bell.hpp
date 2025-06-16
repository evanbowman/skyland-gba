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

#include "decoration.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void make_construction_effect(Vec2<Fixnum> pos);



class Bell final : public Decoration
{
public:
    Bell(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void update(Time delta) override
    {
        Room::update(delta);

        if (chime_count_) {
            Room::ready();
            timer_ += delta;
            if (timer_ >= chime_spacing_) {
                timer_ -= chime_spacing_;
                --chime_count_;
                ring();
                if (chime_repeat_) {
                    repeat_on_ = true;
                    reps_completed_ = 0;
                    repeat_timer_ = 0;
                }
            }
        }

        if (repeat_on_) {
            Room::ready();

            const auto repeat_interval = milliseconds(350);

            repeat_timer_ += delta;
            if (repeat_timer_ >= repeat_interval) {
                repeat_timer_ -= repeat_interval;

                ring();
                ++reps_completed_;

                if (reps_completed_ == chime_repeat_) {
                    repeat_on_ = false;
                }
            }
        }
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::bell;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        render_interior(app, buffer);
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_bell)->c_str();
    }


    static const char* name()
    {
        return "bell";
    }


    static SystemString ui_name()
    {
        return SystemString::block_bell;
    }



    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::locked_by_default | RoomProperties::fragile |
               RoomProperties::multiplayer_unsupported;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 4280;
    }


    static Icon unsel_icon()
    {
        return 4264;
    }


    virtual ScenePtr select_impl(const RoomCoord& cursor) override
    {
        ring();
        return null_scene();
    }


    void ring()
    {
        PLATFORM.speaker().play_sound("ship_bell.raw", 7);
        auto p = visual_center();
        p.y -= 1.0_fixed;
        make_construction_effect(p);
    }


    void schedule_chimes(Time chime_spacing,
                         u8 chime_count,
                         u8 chime_repeat,
                         Time start_delay)
    {
        timer_ = chime_spacing - start_delay;
        chime_count_ = chime_count;
        chime_spacing_ = chime_spacing;
        chime_repeat_ = chime_repeat;
        repeat_on_ = false;
        reps_completed_ = 0;
        Room::ready();
    }


private:
    Time timer_ = 0;
    Time repeat_timer_ = 0;

    Time chime_spacing_ = 0;
    u8 chime_count_ = 0;

    bool repeat_on_ = false;
    u8 reps_completed_ = 0;
    u8 chime_repeat_ = 0;
};



} // namespace skyland
