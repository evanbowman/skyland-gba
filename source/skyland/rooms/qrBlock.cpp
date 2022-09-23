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


#include "qrBlock.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene/qrViewerScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland
{


ScenePtr<Scene>
QrBlock::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    pfrm.speaker().play_sound("button_wooden", 3);

    auto next = scene_pool::alloc<QRViewerScene>(
        data_->c_str(),
        "",
        scene_pool::make_deferred_scene<InspectP2Scene>(),
        ColorConstant::rich_black);

    next->set_origin_overworld();

    return next;
}


} // namespace skyland
