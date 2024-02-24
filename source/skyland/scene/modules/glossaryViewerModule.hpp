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


    ScenePtr<Scene> update(Time delta) override;


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


    ScenePtr<Scene> show_categories_impl(Time);


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
    Time unfade_timer_ = 0;
    Time timer_ = 0;

    using FilterBuf = Buffer<MetaclassIndex, 100>;
    Optional<DynamicMemory<FilterBuf>> filter_buf_;

    static Factory factory_;
};



} // namespace skyland
