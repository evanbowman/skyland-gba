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
#include "qr.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland
{



class QRViewerScene : public Scene
{
public:
    QRViewerScene(const QRCode& qr,
                  DeferredScene next,
                  ColorConstant exit_color);

    QRViewerScene(const char* text,    // Text to encode
                  const char* message, // message to display alongside qr code
                  DeferredScene next,
                  ColorConstant exit_color);


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void set_origin_overworld()
    {
        overworld_ = true;
    }


protected:
    std::optional<QRCode> qr_;
    u32 binary_data_size_ = 0;
    StringBuffer<80> text_;
    StringBuffer<70> message_;
    DeferredScene next_;
    bool exit_ = false;
    bool overworld_ = false;
    std::optional<TextView> tv_;
    std::optional<Text> next_text_;
    ColorConstant exit_color_;
    Microseconds timer_ = 0;
};



class ConfiguredURLQRViewerScene : public QRViewerScene
{
public:
    ConfiguredURLQRViewerScene(
        const char* config_path,
        const char* text,
        const char* message,
        DeferredScene next,
        ColorConstant exit_color = custom_color(0x392194));


    void enter(Platform& pfrm, App& app, Scene& prev) override;


private:
    StringBuffer<64> config_path_;
};



} // namespace skyland
