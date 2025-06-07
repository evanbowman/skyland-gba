////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "sramFileWritebackScene.hpp"
#include "modules/fileBrowserModule.hpp"
#include "platform/flash_filesystem.hpp"



namespace skyland
{



SramFileWritebackScene::SramFileWritebackScene(const char* path,
                                               Vector<char>&& text_buffer,
                                               UserContext&& user_context)
    : path_(path), text_buffer_(std::move(text_buffer)),
      user_context_(std::move(user_context))
{
}



ScenePtr SramFileWritebackScene::update(Time delta)
{
    flash_filesystem::store_file_data_text(
        path_.c_str(), text_buffer_, {.use_compression_ = true});

    return make_scene<FileBrowserModule>(
        std::move(user_context_), path_.c_str(), true);
}



void SramFileWritebackScene::enter(Scene& prev)
{
}



void SramFileWritebackScene::exit(Scene& next)
{
}



} // namespace skyland
