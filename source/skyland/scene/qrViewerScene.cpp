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


#include "skyland/player/playerP1.hpp"
#include "skyland/skyland.hpp"
#include "graphics/overlay.hpp"
#include "platform/platform.hpp"
#include "qrViewerScene.hpp"
#include "qr.hpp"



namespace skyland
{



QRViewerScene::QRViewerScene(const char* text,
                             const char* message,
                             DeferredScene next) :
    message_(message),
    next_(next)
{
    if (str_len(text) < text_.remaining()) {
        text_ = text;
    }
}



void QRViewerScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.load_overlay_texture("overlay");

    if (not text_.empty()) {
        if (auto code = QRCode::create(text_.c_str())) {
            const auto st = calc_screen_tiles(pfrm);

            auto margin = (st.y - code->size() / 2) / 2;
            code->draw(pfrm, {2, (u8)margin});
        }
    }

    pfrm.screen().schedule_fade(1.f, ColorConstant::silver_white);
}



void QRViewerScene::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.load_overlay_texture("overlay");
}



ScenePtr<Scene> QRViewerScene::update(Platform& pfrm,
                                      App& app,
                                      Microseconds delta)
{
    if (exit_) {
        return next_();
    }

    if (player(app).key_down(pfrm, Key::action_1)) {
        exit_ = true;
        pfrm.fill_overlay(0);
        pfrm.screen().schedule_fade(1.f);
    }

    return null_scene();
}




ConfiguredURLQRViewerScene::ConfiguredURLQRViewerScene(const char* config_path,
                                                       const char* text,
                                                       const char* message,
                                                       DeferredScene next)
    : QRViewerScene(text, message, next),
      config_path_(config_path)
{
}



void ConfiguredURLQRViewerScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    auto v = app.invoke_script(pfrm, config_path_.c_str());
    if (v->type() not_eq lisp::Value::Type::string) {
        Platform::fatal("url lisp script returned non-string result");
    }

    // Prepend the url from config.
    auto temp = text_;
    text_.clear();
    text_ = v->string().value();
    text_ += temp;

    info(pfrm, text_);

    QRViewerScene::enter(pfrm, app, prev);
}



}
