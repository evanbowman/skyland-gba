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
#include "skyland/scene/module.hpp"
#include "string.hpp"



namespace skyland
{



class DlcManagerModule : public Module<DlcManagerModule>
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    static SystemString module_name()
    {
        return SystemString::module_dlc_manager;
    }


    static u16 icon()
    {
        return 1192;
    }


    static bool run_scripts()
    {
        return false;
    }


private:
    void show();


    using PatchName = StringBuffer<30>;

    struct PatchInfo
    {
        PatchName name_;
        u8 tiles_used_ : 4;
        u8 sprites_used_ : 4;
    };

    struct PatchList
    {
        Buffer<PatchInfo, 32> list_;
    };

    u32 index_ = 0;

    Optional<Text> patch_name_;
    Optional<Text> tiles_text_;
    Optional<Text> sprites_text_;

    Optional<Text> total_tiles_;
    Optional<Text> total_sprites_;

    Optional<Text> total_tiles_label_;
    Optional<Text> total_sprites_label_;

    Optional<Text> erase_text_;

    Optional<DynamicMemory<PatchList>> patches_;


    static Factory factory_;
};



} // namespace skyland
