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

#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland
{



class QRViewerScene : public Scene
{
public:

    QRViewerScene(const char* text, // Text to encode
                  const char* message, // message to display alongside qr code
                  DeferredScene next);


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


protected:
    StringBuffer<100> text_;
    StringBuffer<84> message_;
    DeferredScene next_;
    bool exit_ = false;
};



class ConfiguredURLQRViewerScene : public QRViewerScene
{
public:

    ConfiguredURLQRViewerScene(const char* config_path,
                               const char* text,
                               const char* message,
                               DeferredScene next);


    void enter(Platform& pfrm, App& app, Scene& prev) override;


private:
    StringBuffer<64> config_path_;
};



}
