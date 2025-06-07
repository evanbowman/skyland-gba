////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "qrBlock.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene/qrViewerScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland
{



void QrBlock::update(Time delta)
{
    Room::update(delta);

    if (hint_img_release_timer_ > 0) {
        hint_img_release_timer_ -= delta;

        if (hint_img_release_timer_ <= 0) {
            hint_img_.reset();
        }
    }

    if (hint_img_) {
        Room::ready();
    }
}



ScenePtr QrBlock::select_impl(const RoomCoord& cursor)
{
    PLATFORM.speaker().play_sound("button_wooden", 3);

    auto next = make_scene<QRViewerScene>(data_->c_str(),
                                          "",
                                          make_deferred_scene<InspectP2Scene>(),
                                          ColorConstant::rich_black);

    next->set_origin_overworld();

    return next;
}



bool QrBlock::opponent_display_on_hover() const
{
    return true;
}



void QrBlock::display_on_hover(Platform::Screen& screen,

                               const RoomCoord& cursor)
{
    if (not hint_img_) {
        for (auto& room : parent()->rooms()) {
            if (auto qr = room->cast<QrBlock>()) {
                if (qr->hint_img_) {
                    hint_img_ = *qr->hint_img_;
                    return;
                }
            }
        }
        hint_img_ = Platform::instance().make_dynamic_texture();
        if (not hint_img_) {
            return;
        }
        (*hint_img_)->remap(82 * 2);
        return;
    }

    Sprite spr;
    auto pos = visual_center();
    pos.y -= 22.0_fixed;
    pos.x -= 12.0_fixed;
    spr.set_texture_index((*hint_img_)->mapping_index());
    spr.set_position(pos);
    screen.draw(spr);

    Room::ready();

    hint_img_release_timer_ = milliseconds(100);
}



void QrBlock::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::qr_block;
}



void QrBlock::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::qr_block;
}



} // namespace skyland
