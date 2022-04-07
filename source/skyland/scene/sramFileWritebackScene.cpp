////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "sramFileWritebackScene.hpp"
#include "modules/fileBrowserModule.hpp"
#include "platform/ram_filesystem.hpp"



namespace skyland
{



SramFileWritebackScene::SramFileWritebackScene(const char* path,
                                               Vector<char>&& text_buffer,
                                               UserContext&& user_context)
    : path_(path), text_buffer_(std::move(text_buffer)),
      user_context_(std::move(user_context))
{
}



ScenePtr<Scene>
SramFileWritebackScene::update(Platform& pfrm, App&, Microseconds delta)
{
    ram_filesystem::store_file_data(pfrm, path_.c_str(), text_buffer_);

    return scene_pool::alloc<FileBrowserModule>(
        std::move(user_context_), path_.c_str(), true);
}



void SramFileWritebackScene::enter(Platform&, App&, Scene& prev)
{
}



void SramFileWritebackScene::exit(Platform&, App&, Scene& next)
{
}



} // namespace skyland
