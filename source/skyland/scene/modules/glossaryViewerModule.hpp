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


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void set_next_scene(DeferredScene next)
    {
        next_scene_.emplace(next);
    }


    static bool stop_sound()
    {
        return false;
    }


    bool disable_backdrop_ = false;
    bool inspect_ = false;
    bool disable_fade_on_exit_ = false;

private:
    void load_page(int page);

    void load_categories();

    void load_filters();


    ScenePtr show_categories_impl(Time);


    Optional<Text> item_name_;
    Optional<Text> item_details_;
    Optional<TextView> item_description_;
    Optional<Text> dependency_text_;

    void show_category_image(int img);

    Optional<DeferredScene> next_scene_;

    void draw_category_line(int line, Text::OptColors = {});

    enum class State {
        show_categories,
        filters,
        view_filtered,
        view,
        quickview,
        exit,
        swap_category_image,
        category_transition_out,
        category_transition_in,
        category_transition_enter,
        fadeout,
    } state_ = State::category_transition_enter;

    int page_ = 0;
    int cg_cursor_ = 0;
    int filter_cursor_ = 0;

    int filter_begin_ = 0;
    int filter_end_ = 0;

    int cover_img_ = 0;
    Time timer_ = 0;

    using FilterBuf = Buffer<MetaclassIndex, 100>;
    Optional<DynamicMemory<FilterBuf>> filter_buf_;

    static Factory factory_;
};



} // namespace skyland
