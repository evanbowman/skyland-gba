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
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern SharedVariable energy_glow_color;
extern SharedVariable spr_energy_color_1;
extern SharedVariable spr_energy_color_2;



class TuningCrystal final : public Decoration
{
public:
    TuningCrystal(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::tuning_crystal;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        render_interior(app, buffer);
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_tuning_crystal)->c_str();
    }


    static const constexpr char* name()
    {
        return "tuning-crystal";
    }


    static SystemString ui_name()
    {
        return SystemString::block_tuning_crystal;
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


    static Icon unsel_icon()
    {
        return 3096;
    }


    static Icon icon()
    {
        return 3080;
    }


    void update(Time delta) override
    {
        Room::update(delta);

        if (note_stop_timer_ > 0) {
            Room::ready();
            note_stop_timer_ -= delta;
            if (note_stop_timer_ <= 0) {
                psg_stop_all();
            }
        }
    }


    ScenePtr select_impl(const RoomCoord& cursor) override
    {
        auto opts = APP.invoke_script("/scripts/data/tuning-crystal.lisp");
        auto count = lisp::length(opts);
        selected_color_++;
        if (selected_color_ >= count) {
            selected_color_ = 0;
        }
        auto elem = lisp::get_list(opts, selected_color_);
        energy_glow_color.set(lisp::get_list(elem, 1)->integer().value_);
        spr_energy_color_1.set(lisp::get_list(elem, 2)->integer().value_);
        spr_energy_color_2.set(lisp::get_list(elem, 3)->integer().value_);
        parent()->schedule_repaint();
        auto freq = lisp::get_list(elem, 5)->integer().value_;
        auto oct = lisp::get_list(elem, 6)->integer().value_;
        Platform::Speaker::NoteDesc n;
        n.regular_.note_ = (Platform::Speaker::Note)freq;
        n.regular_.octave_ = oct;
        PLATFORM_EXTENSION(
            psg_play_note, Platform::Speaker::Channel::square_1, n);
        note_stop_timer_ = seconds(1);
        Room::ready();
        return null_scene();
    }


    void finalize() override
    {
        Room::finalize();
        psg_stop_all();
    }


private:
    int selected_color_ = 0;
    Time note_stop_timer_ = 0;
};



} // namespace skyland
