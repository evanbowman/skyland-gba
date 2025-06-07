////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "scene_pool.hpp"
#include "scene/bootScene.hpp"



namespace skyland
{


namespace scene_pool
{


_Pool* pool_ = nullptr;


}


ScenePtr null_scene()
{
    return {nullptr, scene_pool::deleter};
}



ScenePtr initial_scene(bool clean_boot)
{
    return make_scene<BootScene>(clean_boot);
}


} // namespace skyland
