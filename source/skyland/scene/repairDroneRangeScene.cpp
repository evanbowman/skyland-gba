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


#include "repairDroneRangeScene.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



ScenePtr<Scene> RepairDroneRangeScene::update(Microseconds delta)
{
    if (drone_->destination() not_eq &APP.player_island()) {
        far_camera();
    }

    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    if (not description_) {
        description_.emplace(SYSTR(repair_range)->c_str(), OverlayCoord{0, 19});

        for (int i = 0; i < description_->len(); ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 18, 425);
        }
    }

    if (APP.player().key_down(Key::action_2)) {
        description_.reset();
        PLATFORM.fill_overlay(0);

        if (is_far_camera()) {
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
        }
    }

    return null_scene();
}



void RepairDroneRangeScene::display()
{
    WorldScene::display();

    auto pos = drone_->position();

    auto origin = drone_->destination()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y - 2) * 16)});
        PLATFORM.screen().draw(sprite);


        if (x not_eq pos.x) {
            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);

            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y) * 16)});

            PLATFORM.screen().draw(sprite);
            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_texture_index(13);
        }


        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y + 1) * 16)});

        PLATFORM.screen().draw(sprite);
    }
}



} // namespace skyland
