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


#include "scene_pool.hpp"
#include "scene/introCreditsScene.hpp"
#include "scene/introCutsceneScene.hpp"
#include "scene/qrViewerScene.hpp"



namespace skyland
{


namespace scene_pool
{


_Pool* pool_ = nullptr;


}


ScenePtr<Scene> null_scene()
{
    return {nullptr, scene_pool::deleter};
}


ScenePtr<Scene> initial_scene()
{
    // "skyland.github.io/hs?sc=100000000&m=2&v=255&t=fffffff"
    return scene_pool::alloc<ConfiguredURLQRViewerScene>("/scripts/config/uploadscore.lisp",
                                                         "?sc=100000000&m=2&v=255&t=fffffff",
                                            "scan me :)",
                                            scene_pool::make_deferred_scene<IntroCreditsScene>());
    // return scene_pool::alloc<IntroCreditsScene>();
}


} // namespace skyland
