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

#include "graphics/overlay.hpp"
#include "skyland/room.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class GlossaryViewerModule : public Module<GlossaryViewerModule>
{
public:
    GlossaryViewerModule(int page = 0) : page_(page)
    {
    }


    void skip_categories()
    {
        state_ = State::quickview;
    }


    static SystemString module_name()
    {
        return SystemString::module_glossary;
    }


    static u16 icon()
    {
        return 1304;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void set_next_scene(DeferredScene next)
    {
        next_scene_.emplace(next);
    }


    static bool stop_sound()
    {
        return false;
    }


    bool inspect_ = false;

private:
    void load_page(Platform& pfrm, int page);

    void load_categories(Platform& pfrm);

    void load_filters(Platform& pfrm);


    std::optional<Text> item_name_;
    std::optional<Text> item_details_;
    std::optional<TextView> item_description_;
    std::optional<Text> dependency_text_;

    void show_category_image(Platform& pfrm, int img);

    std::optional<DeferredScene> next_scene_;

    void draw_category_line(Platform&, int line,
                            Text::OptColors = {});

    enum class State {
        show_categories,
        filters,
        view_filtered,
        view,
        quickview,
        exit,
        swap_category_image,
    } state_ = State::show_categories;

    int page_ = 0;
    int cg_cursor_ = 0;
    int filter_cursor_ = 0;

    int filter_begin_ = 0;
    int filter_end_ = 0;

    Microseconds img_swap_timer_ = 0;

    Microseconds unfade_timer_ = 0;

    using FilterBuf = Buffer<MetaclassIndex, 100>;
    std::optional<DynamicMemory<FilterBuf>> filter_buf_;

    static Factory factory_;
};



} // namespace skyland
