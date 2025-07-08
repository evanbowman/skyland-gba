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
#include "script/lisp.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



QRViewerScene::QRViewerScene(const char* text,
                             const char* message,
                             DeferredScene next,
                             ColorConstant exit_color)
    : message_(message), next_(next), exit_color_(exit_color),
      text_(allocate_dynamic<TextBuffer>("qr-text-buffer"))
{
    **text_ = text;
}



QRViewerScene::QRViewerScene(const QRCode& qr,
                             DeferredScene next,
                             ColorConstant exit_color)
    : message_(""), next_(next), exit_color_(exit_color)
{
    qr_ = qr;
}



void QRViewerScene::enter(Scene& prev)
{
    PLATFORM.load_overlay_texture("overlay_qr");

    tv_.emplace();

    if (qr_ or (text_ and not(*text_)->empty())) {

        if (not overworld_) {
            auto str = SYSTR(qr_prep);
            auto m = centered_text_margins(utf8::len(str->c_str()));
            Text t(OverlayCoord{(u8)m, (u8)(calc_screen_tiles().y / 2 - 1)});
            t.assign(str->c_str(),
                     OptColors{{ColorConstant::silver_white,
                                custom_color(0x006ea6)}});
            PLATFORM_EXTENSION(force_vsync);
            PLATFORM.screen().fade(1.f, custom_color(0x006ea6));
            PLATFORM.screen().display();
        }

        if (not qr_ and text_ and not(*text_)->empty()) {
            qr_ = QRCode::create((*text_)->c_str());
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

            PLATFORM.screen().display();


            u8 margin = 1;

            if (qr_->drawsize(format_) <= 38 / 2) {
                const auto st = calc_screen_tiles();

                margin = (st.y - qr_->drawsize(format_)) / 2;

                int lc = [&] {
                    return tv_->assign(
                        message_.c_str(),
                        {u8(5 + qr_->drawsize(format_)), 1},
                        {u8(st.x - (6 + qr_->drawsize(format_))), 18},
                        0);
                }();

                PLATFORM.fill_overlay(0);



                u8 text_margin = (st.y - lc) / 2;
                tv_->assign(message_.c_str(),
                            {u8(5 + qr_->drawsize(format_)), text_margin},
                            {u8(st.x - (6 + qr_->drawsize(format_))), 18},
                            0,
                            OptColors{{custom_color(0x006ea6),
                                       ColorConstant::silver_white}});

                if (overworld_) {
                    PLATFORM.fill_overlay(0);
                }

                auto next_str = SYSTR(a_next);

                u8 next_start = st.x - utf8::len(next_str->c_str());
                next_text_.emplace(OverlayCoord{next_start, 19});
                next_text_->assign(
                    next_str->c_str(),
                    OptColors{{overworld_ ? custom_color(0x4e4e73)
                                          : ColorConstant::silver_white,
                               overworld_ ? custom_color(0xd2d9a7)
                                          : custom_color(0x006ea6)}});
            }

            qr_->draw({2, (u8)margin}, (int)format_);
        } else {
            Platform::fatal("qr gen failed");
        }
    }

    PLATFORM.screen().schedule_fade(
        1.f,
        {overworld_ ? custom_color(0x4e4e73) : ColorConstant::silver_white});
}



void QRViewerScene::exit(Scene& next)
{
    PLATFORM.load_overlay_texture("overlay");
    next_text_.reset();
}



ScenePtr QRViewerScene::update(Time delta)
{
    if (exit_) {
        return next_();
    }

    if (overworld_) {
        APP.level_timer().count_up(delta);
    }

    timer_ += delta;
    if (timer_ < 0) { // overflow? Would take 30 minutes or so, but...
        timer_ = 0;
    }

    if (timer_ > milliseconds(300) and player().key_down(Key::action_1)) {
        exit_ = true;
        tv_.reset();
        next_text_.reset();
        PLATFORM.fill_overlay(0);
        if (overworld_) {
            PLATFORM.screen().schedule_fade(0.f);
        } else {
            PLATFORM.screen().schedule_fade(1.f, {.color = exit_color_});
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
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



void ConfiguredURLQRViewerScene::enter(Scene& prev)
{
    // NOTE: enabling developer mode does not allow the player to record
    // highscores. But, we do want the software to be resiliant to future
    // changes, and we make a temporary exception, allowing users to run custom
    // scripts if and only if we're reading the config script for the highscore
    // server url.
    const bool was_developer_mode = APP.is_developer_mode();
    APP.set_developer_mode(true);

    auto v = APP.invoke_script(config_path_.c_str());
    if (v->type() not_eq lisp::Value::Type::string) {
        Platform::fatal("url lisp script returned non-string result");
    }

    APP.set_developer_mode(was_developer_mode);

    auto temp = allocate_dynamic<StringBuffer<500>>("temp-buf-qr");
    // Prepend the url from config.
    *temp = (*text_)->c_str();
    (*text_)->clear();
    **text_ = v->string().value();
    **text_ += *temp;

    QRViewerScene::enter(prev);
}



} // namespace skyland
