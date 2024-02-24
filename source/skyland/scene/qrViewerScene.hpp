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


    ScenePtr<Scene> update(Time delta) override;


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
        ColorConstant exit_color = custom_color(0x392194));


    void enter(Scene& prev) override;


private:
    StringBuffer<64> config_path_;
};



} // namespace skyland
