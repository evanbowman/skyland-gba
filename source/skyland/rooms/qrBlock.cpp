////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


#include "qrBlock.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene/qrViewerScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland
{



void QrBlock::update(Microseconds delta)
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



ScenePtr<Scene> QrBlock::select(const RoomCoord& cursor)
{
    PLATFORM.speaker().play_sound("button_wooden", 3);

    auto next = scene_pool::alloc<QRViewerScene>(
        data_->c_str(),
        "",
        scene_pool::make_deferred_scene<InspectP2Scene>(),
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
