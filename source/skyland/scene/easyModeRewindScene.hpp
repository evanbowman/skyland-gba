#pragma once

#include "readyScene.hpp"
#include "rewindScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"


namespace skyland
{



class EasyModeRewindScene : public WorldScene
{
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        WorldScene::enter(pfrm, app, prev);

        pfrm.load_overlay_texture("overlay_challenges");

        yes_text_.emplace(pfrm, OverlayCoord{3, 7});
        no_text_.emplace(pfrm, OverlayCoord{3, 9});

        yes_text_->assign(SYSTR(yes)->c_str());
        no_text_->assign(SYSTR(no)->c_str());

        auto title_str = SYSTR(easy_mode_auto_rewind_title);
        u8 mg = centered_text_margins(pfrm, utf8::len(title_str->c_str()));

        title_.emplace(pfrm, title_str->c_str(), OverlayCoord{mg, 1});

        text_.emplace(pfrm,
                      SYSTR(easy_mode_auto_rewind_text)->c_str(),
                      OverlayCoord{1, 5});

        pfrm.screen().pixelate(128, false);
        pfrm.screen().schedule_fade(0.7f);
    }


    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        pfrm.fill_overlay(0);
        pfrm.screen().clear();
        pfrm.screen().display();

        pfrm.load_overlay_texture("overlay");
        WorldScene::exit(pfrm, app, next);

        title_.reset();
        text_.reset();
        yes_text_.reset();
        no_text_.reset();

        pfrm.screen().pixelate(0, false);
        pfrm.screen().schedule_fade(0.f);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (app.player().key_down(pfrm, Key::up)) {
            if (selected_ not_eq 0) {
                pfrm.speaker().play_sound("click_wooden", 2);
            }
            selected_ = 0;
        } else if (app.player().key_down(pfrm, Key::down)) {
            if (selected_ not_eq 1) {
                pfrm.speaker().play_sound("click_wooden", 2);
            }
            selected_ = 1;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            switch (selected_) {
            case 0:
                state_bit_store(
                    app, StateBit::easy_mode_rewind_declined, false);
                return scene_pool::alloc<RewindScene>(false);

            case 1:
                state_bit_store(app, StateBit::easy_mode_rewind_declined, true);
                return scene_pool::alloc<ReadyScene>();
            }
        }



        // NOTE: because the procgen ai forcibly sets level number in top
        // corner.
        pfrm.set_tile(Layer::overlay, calc_screen_tiles(pfrm).x - 1, 0, 0);
        pfrm.set_tile(Layer::overlay, calc_screen_tiles(pfrm).x - 2, 0, 0);

        if (selected_ == 0) {
            pfrm.set_tile(Layer::overlay, 1, yes_text_->coord().y, 87);
            pfrm.set_tile(Layer::overlay, 1, no_text_->coord().y, 0);
        } else {
            pfrm.set_tile(Layer::overlay, 1, yes_text_->coord().y, 0);
            pfrm.set_tile(Layer::overlay, 1, no_text_->coord().y, 87);
        }


        return null_scene();
    }

private:
    std::optional<Text> title_;
    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;

    int selected_ = 0;
};



} // namespace skyland
