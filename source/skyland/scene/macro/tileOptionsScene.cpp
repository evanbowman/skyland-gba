////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "tileOptionsScene.hpp"
#include "createBlockScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void TileOptionsScene::enter(macro::EngineImpl& state, Scene& prev)
{
    MacrocosmScene::enter(state, prev);
    collect_options(state);
    show_options(state);
}



void TileOptionsScene::exit(macro::EngineImpl& state, Scene& next)
{
    MacrocosmScene::exit(state, next);
    text_.reset();

    const auto st = calc_screen_tiles();
    for (int y = st.y - 8; y < st.y; ++y) {
        for (int x = 0; x < 32; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }
}



struct TileOptionsScene::OptionInfo
{
    SystemString name_;
    int sel_icon_;
    int unsel_icon_;
    void (*render_cost_)(macro::EngineImpl&, terrain::Type, Text&);
    ScenePtr (*next_)(MacrocosmScene&, macro::EngineImpl&);
};



void render_cost(macro::EngineImpl& state,
                 terrain::Type t,
                 Text& text,
                 bool harvest,
                 Text::OptColors text_colors = {},
                 Optional<terrain::Cost> inp_c = {});



static const TileOptionsScene::OptionInfo options[] = {
    {SystemString::macro_create_block,
     2568,
     2584,
     [](macro::EngineImpl&, terrain::Type, Text&) {},
     [](MacrocosmScene& s, macro::EngineImpl& state) -> ScenePtr {
         return make_scene<CreateBlockScene>();
     }},
    {SystemString::macro_build_improvement,
     2520,
     2536,
     [](macro::EngineImpl&, terrain::Type, Text&) {},
     [](MacrocosmScene& s, macro::EngineImpl& state) -> ScenePtr {
         return make_scene<BuildImprovementScene>();
     }},
    {SystemString::macro_demolish,
     2600,
     2616,
     [](macro::EngineImpl& state, terrain::Type t, Text& text) {
         render_cost(state, t, text, true);
     },
     [](MacrocosmScene& s, macro::EngineImpl& state) -> ScenePtr {
         auto c = state.sector().cursor();
         c.z--;
         auto tp = state.sector().get_block(c).type();
         if (tp == terrain::Type::dynamite) {
             state.sector().ref_block(c).data_ = 1;
             return make_scene<SelectorScene>();
         } else if (not harvest_block(state, state.sector(), c)) {
             Platform::instance().speaker().play_sound("beep_error", 2);
             return make_scene<TileOptionsScene>();
         } else {
             s.update_ui(state);
             return make_scene<SelectorScene>();
         }
     }}};


ScenePtr TileOptionsScene::update(Player& player, macro::EngineImpl& state)
{
    if (auto scene = MacrocosmScene::update(player, state)) {
        return scene;
    }

    if (state.sector().cursor().z == 0) {
        // If z == 0, no tiles beneath, so you can't improve or remove a
        // block. Go directly to the block creation menu.
        last_option_ = &options[0];
        return make_scene<CreateBlockScene>();
    } else {
        auto c = state.sector().cursor();
        --c.z;
        auto& beneath = state.sector().get_block(c);
        if (beneath.type() == terrain::Type::air) {
            last_option_ = &options[0];
            return make_scene<CreateBlockScene>();
        }
    }

    if (player.key_down(Key::action_1)) {
        PLATFORM.speaker().play_sound("button_wooden", 3);
        auto next = options_[selector_]->next_(*this, state);
        last_option_ = options_[selector_];
        return next;
    }

    if (player.key_down(Key::action_2)) {
        return make_scene<SelectorScene>();
    }

    if (player.key_down(Key::left)) {
        if (selector_ == 0) {
            selector_ = options_.size() - 1;
        } else {
            --selector_;
        }
        show_options(state);
        PLATFORM.speaker().play_sound("click", 1);
    }

    if (player.key_down(Key::right)) {
        ++selector_;
        selector_ %= options_.size();
        show_options(state);
        PLATFORM.speaker().play_sound("click", 1);
    }

    return null_scene();
}



const TileOptionsScene::OptionInfo* TileOptionsScene::last_option_ =
    &options[1];



void TileOptionsScene::collect_options(macro::EngineImpl& state)
{

    auto c = state.sector().cursor();
    if (c.z == 0) {
        return;
    }
    --c.z;
    auto& block = state.sector().get_block(c);
    auto improvements = terrain::improvements((terrain::Type)block.type_);
    if (not improvements.empty()) {
        options_.push_back(&options[1]);
    }

    options_.push_back(&options[0]);

    options_.push_back(&options[2]);

    for (u32 i = 0; i < options_.size(); ++i) {
        if (options_[i] == last_option_) {
            selector_ = i;
        }
    }
}



void TileOptionsScene::show_options(macro::EngineImpl& state)
{
    if (options_.empty()) {
        return;
    }

    auto st = calc_screen_tiles();

    StringBuffer<32> str = loadstr(options_[selector_]->name_)->c_str();
    msg(state, str.c_str());

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        PLATFORM.set_tile(Layer::overlay, st.x - 22, y, 130);
        PLATFORM.set_tile(Layer::overlay, st.x - 9, y, 433);
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 22, st.y - 2, 419);
    PLATFORM.set_tile(Layer::overlay, st.x - 9, st.y - 2, 418);

    PLATFORM.load_overlay_chunk(
        258, options_[(selector_ + 1) % options_.size()]->unsel_icon_, 16);

    int sel = selector_;
    if (sel - 1 < 0) {
        sel = options_.size() - 1;
    } else {
        sel -= 1;
    }
    PLATFORM.load_overlay_chunk(181, options_[sel]->unsel_icon_, 16);

    PLATFORM.load_overlay_chunk(
        197, options_[(selector_) % options_.size()]->sel_icon_, 16);

    for (int i = st.x - 21; i < st.x - 9; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 6, 425);
    }

    draw_image(181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);
    draw_image(197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);
    draw_image(258, st.x - 13, st.y - 5, 4, 4, Layer::overlay);
}



void TileOptionsScene::msg(macro::EngineImpl& state, const char* text)
{
    auto st = calc_screen_tiles();
    text_.emplace(text, OverlayCoord{0, u8(st.y - 1)});

    if (state.sector().cursor().z > 0) {
        auto c = state.sector().cursor();
        --c.z;
        auto t = state.sector().get_block(c).type();
        options_[selector_]->render_cost_(state, t, *text_);
    }


    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 3, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 5, 0);

        PLATFORM.set_tile(Layer::overlay, i, st.y - 6, 0);
    }
}



} // namespace skyland::macro
