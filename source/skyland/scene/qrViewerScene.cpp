///////////////////////////////////////////////////////////////////////////////
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


#include "qrViewerScene.hpp"
#include "platform/platform.hpp"
#include "qr.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



QRViewerScene::QRViewerScene(const char* text,
                             const char* message,
                             DeferredScene next,
                             ColorConstant exit_color)
    : message_(message), next_(next), exit_color_(exit_color)
{
    if (str_len(text) < text_.remaining()) {
        text_ = text;
    }
}



QRViewerScene::QRViewerScene(const QRCode& qr,
                             DeferredScene next,
                             ColorConstant exit_color)
    : message_(""), next_(next), exit_color_(exit_color)
{
    qr_ = qr;
}



void QRViewerScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.load_overlay_texture("overlay_qr");

    tv_.emplace(pfrm);

    if (qr_ or not text_.empty()) {

        if (not overworld_) {
            auto str = SYSTR(qr_prep);
            auto m = centered_text_margins(pfrm, utf8::len(str->c_str()));
            Text t(
                pfrm,
                OverlayCoord{(u8)m, (u8)(calc_screen_tiles(pfrm).y / 2 - 1)});
            t.assign(str->c_str(),
                     OptColors{{ColorConstant::silver_white,
                                custom_color(0x392194)}});
            pfrm.system_call("vsync", nullptr);
            pfrm.screen().fade(1.f, custom_color(0x392194));
            pfrm.screen().display();
        }

        if (not qr_ and not text_.empty()) {
            qr_ = QRCode::create(text_.c_str());
        }

        if (qr_) {

            if (overworld_) {
                qr_->data_color_index(10)
                    .position_marker_inner_color_index(10)
                    .position_marker_outer_color_index(10);
            } else {
                qr_->data_color_index(7);
                qr_->position_marker_outer_color_index(7);
                qr_->position_marker_inner_color_index(7);
            }

            pfrm.screen().display();


            u8 margin = 1;

            if (qr_->drawsize(format_) <= 38 / 2) {
                const auto st = calc_screen_tiles(pfrm);

                margin = (st.y - qr_->drawsize(format_)) / 2;

                int lc = [&] {
                    return tv_->assign(
                        message_.c_str(),
                        {u8(5 + qr_->drawsize(format_)), 1},
                        {u8(st.x - (6 + qr_->drawsize(format_))), 18},
                        0);
                }();

                pfrm.fill_overlay(0);



                u8 text_margin = (st.y - lc) / 2;
                tv_->assign(message_.c_str(),
                            {u8(5 + qr_->drawsize(format_)), text_margin},
                            {u8(st.x - (6 + qr_->drawsize(format_))), 18},
                            0,
                            OptColors{{custom_color(0x392194),
                                       ColorConstant::silver_white}});

                if (overworld_) {
                    pfrm.fill_overlay(0);
                }

                auto next_str = SYSTR(a_next);

                u8 next_start = st.x - utf8::len(next_str->c_str());
                next_text_.emplace(pfrm, OverlayCoord{next_start, 19});
                next_text_->assign(
                    next_str->c_str(),
                    OptColors{{overworld_ ? custom_color(0x4e4e73)
                                          : ColorConstant::silver_white,
                               overworld_ ? custom_color(0xd2d9a7)
                                          : custom_color(0x392194)}});
            }

            qr_->draw(pfrm, {2, (u8)margin}, (int)format_);
        } else {
            Platform::fatal("qr gen failed");
        }
    }

    pfrm.screen().schedule_fade(
        1.f, overworld_ ? custom_color(0x4e4e73) : ColorConstant::silver_white);
}



void QRViewerScene::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.load_overlay_texture("overlay");
    next_text_.reset();
}



ScenePtr<Scene>
QRViewerScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (exit_) {
        return next_();
    }

    if (overworld_) {
        app.level_timer().count_up(delta);
    }

    timer_ += delta;
    if (timer_ < 0) { // overflow? Would take 30 minutes or so, but...
        timer_ = 0;
    }

    if (timer_ > milliseconds(300) and
        player(app).key_down(pfrm, Key::action_1)) {
        exit_ = true;
        tv_.reset();
        pfrm.fill_overlay(0);
        if (overworld_) {
            pfrm.screen().schedule_fade(0.f);
        } else {
            pfrm.screen().schedule_fade(1.f, exit_color_);
        }
    }

    return null_scene();
}



ConfiguredURLQRViewerScene::ConfiguredURLQRViewerScene(const char* config_path,
                                                       const char* text,
                                                       const char* message,
                                                       DeferredScene next,
                                                       ColorConstant exit_color)
    : QRViewerScene(text, message, next, exit_color), config_path_(config_path)
{
}



void ConfiguredURLQRViewerScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    // NOTE: enabling developer mode does not allow the player to record
    // highscores. But, we do want the software to be resiliant to future
    // changes, and we make a temporary exception, allowing users to run custom
    // scripts if and only if we're reading the config script for the highscore
    // server url.
    const bool was_developer_mode = app.is_developer_mode();
    app.set_developer_mode(true);

    auto v = app.invoke_script(pfrm, config_path_.c_str());
    if (v->type() not_eq lisp::Value::Type::string) {
        Platform::fatal("url lisp script returned non-string result");
    }

    app.set_developer_mode(was_developer_mode);


    // Prepend the url from config.
    auto temp = text_;
    text_.clear();
    text_ = v->string().value();
    text_ += temp;

    QRViewerScene::enter(pfrm, app, prev);
}



} // namespace skyland
