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
#include "skyland/player/player.hpp"
#include "skyland/scene.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class MenuPromptScene : public Scene
{
public:

    using OptCallback = Function<4, void (Platform&, App&)>;


    MenuPromptScene(SystemString msg,
                    SystemString opt_1,
                    SystemString opt_2,
                    DeferredScene next,
                    OptCallback opt_1_callback,
                    OptCallback opt_2_callback) :
        next_(next),
        msg_(msg),
        opt_1_(opt_1),
        opt_2_(opt_2),
        opt_1_callback_(opt_1_callback),
        opt_2_callback_(opt_2_callback)
    {
    }


    static constexpr const auto sel_colors =
        FontColors{custom_color(0x000010), custom_color(0xffffff)};


    void enter(Platform& pfrm, App&, Scene& prev) override
    {
        pfrm.screen().schedule_fade(0);
        pfrm.screen().schedule_fade(1);

        text_.emplace(pfrm);
        text_->assign(loadstr(pfrm, msg_)->c_str(),
                      {1, 1},
                      {28, 14},
                      0);

        t1_.emplace(pfrm,
                    OverlayCoord{3, 16});

        t1_->assign(loadstr(pfrm, opt_1_)->c_str(), sel_colors);

        t2_.emplace(pfrm,
                    loadstr(pfrm, opt_2_)->c_str(),
                    OverlayCoord{3, 18});

        pfrm.set_tile(Layer::overlay, 1, 16, 475);
        pfrm.set_tile(Layer::overlay, 1, 18, 0);
    }


    void exit(Platform&, App&, Scene& next) override
    {
        text_.reset();
        t1_.reset();
        t2_.reset();
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta)
    {
        if (player(app).key_down(pfrm, Key::action_1)) {
            if (cursor_ == 0) {
                opt_1_callback_(pfrm, app);
            } else {
                opt_2_callback_(pfrm, app);
            }
            return next_();
        }

        if (player(app).key_down(pfrm, Key::up)) {
            cursor_ = 0;
            t1_->assign(loadstr(pfrm, opt_1_)->c_str(), sel_colors);
            t2_->assign(loadstr(pfrm, opt_2_)->c_str());
            pfrm.set_tile(Layer::overlay, 1, 16, 475);
            pfrm.set_tile(Layer::overlay, 1, 18, 0);
        }

        if (player(app).key_down(pfrm, Key::down)) {
            cursor_ = 1;
            t1_->assign(loadstr(pfrm, opt_1_)->c_str());
            t2_->assign(loadstr(pfrm, opt_2_)->c_str(), sel_colors);
            pfrm.set_tile(Layer::overlay, 1, 18, 475);
            pfrm.set_tile(Layer::overlay, 1, 16, 0);
        }

        return null_scene();
    }


private:
    DeferredScene next_;
    SystemString msg_;
    SystemString opt_1_;
    SystemString opt_2_;

    std::optional<TextView> text_;
    std::optional<Text> t1_;
    std::optional<Text> t2_;

    int cursor_ = 0;

    OptCallback opt_1_callback_;
    OptCallback opt_2_callback_;
};



}
