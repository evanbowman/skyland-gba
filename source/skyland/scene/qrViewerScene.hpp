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

#include "allocator.hpp"
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


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void set_origin_overworld()
    {
        overworld_ = true;
    }

protected:
    Optional<QRCode> qr_;
    u32 binary_data_size_ = 0;
    using TextBuffer = StringBuffer<600>;
    StringBuffer<70> message_;
    DeferredScene next_;
    Optional<TextView> tv_;
    Optional<Text> next_text_;
    ColorConstant exit_color_;
    Time timer_ = 0;
    bool exit_ = false;
    bool overworld_ = false;
    Optional<DynamicMemory<TextBuffer>> text_;

public:
    u8 format_ = 0;
};



class ConfiguredURLQRViewerScene : public QRViewerScene
{
public:
    ConfiguredURLQRViewerScene(
        const char* config_path,
        const char* text,
        const char* message,
        DeferredScene next,
        ColorConstant exit_color = custom_color(0x006ea6));


    void enter(Scene& prev) override;


private:
    StringBuffer<64> config_path_;
};



} // namespace skyland
