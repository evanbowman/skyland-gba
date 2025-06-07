////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "tnt.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/explosion/exploTrail.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/entity/projectile/fireBolt.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Explosive::Explosive(Island* parent,
                     const RoomCoord& position,
                     const char* class_name)
    : Room(parent, class_name, position)
{
}



void Explosive::rewind(Time delta)
{
    Room::rewind(delta);

    if (health() == max_health()) {
        Room::ready();
        ignition_ = false;
    }
}



void Explosive::update(Time delta)
{
    Room::update(delta);

    if (health() not_eq max_health()) {
        Room::ready();
        ignition_ = true;

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(5, {});
            damage_timer_ = 0;
        }
    }
}



class IgniteExplosiveScene : public ActiveWorldScene
{
public:
    IgniteExplosiveScene(RoomCoord c) : coord_(c)
    {
    }


    void enter(Scene& prev) override
    {
        WorldScene::enter(prev);
        auto st = calc_screen_tiles();

        text_.emplace("ignite?", OverlayCoord{0, u8(st.y - 1)});

        const int count = st.x - text_->len();
        for (int i = 0; i < count; ++i) {
            PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
        }

        for (int i = 0; i < st.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
        }
        yes_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
        no_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

        yes_text_->assign(SYSTR(salvage_option_A)->c_str());
        no_text_->assign(SYSTR(salvage_option_B)->c_str());

        for (int i = 23; i < st.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 425);
        }

        PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
        PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);

        PLATFORM.set_tile(Layer::overlay, 0, st.y - 3, 160);
        PLATFORM.set_tile(Layer::overlay, 1, st.y - 3, 161);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 2, 162);
        PLATFORM.set_tile(Layer::overlay, 1, st.y - 2, 163);

        PLATFORM.set_tile(Layer::overlay, 2, st.y - 2, 418);
        PLATFORM.set_tile(Layer::overlay, 2, st.y - 3, 433);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 4, 425);
        PLATFORM.set_tile(Layer::overlay, 1, st.y - 4, 425);
    }


    void exit(Scene& next) override
    {
        WorldScene::exit(next);

        text_.reset();
        yes_text_.reset();
        no_text_.reset();

        PLATFORM.fill_overlay(0);
    }


    void display() override
    {
        if (auto r = APP.player_island().get_room(coord_)) {
            r->display_on_hover(PLATFORM.screen(), coord_);
        }

        ActiveWorldScene::display();
    }


    ScenePtr update(Time delta) override
    {
        if (auto s = ActiveWorldScene::update(delta)) {
            return s;
        }

        if (APP.player().key_down(Key::action_1)) {
            if (auto r = APP.player_island().get_room(coord_)) {
                r->apply_damage(1);

                network::packet::DynamiteActivated packet;
                packet.x_ = r->position().x;
                packet.y_ = r->position().y;
                network::transmit(packet);
            }
            return make_scene<ReadyScene>();
        }

        if (APP.player().key_down(Key::action_2)) {
            return make_scene<ReadyScene>();
        }

        return null_scene();
    }


private:
    Optional<Text> text_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;
    RoomCoord coord_;
};



ScenePtr Explosive::select_impl(const RoomCoord& cursor)
{
    if (parent() not_eq &APP.player_island()) {
        return null_scene();
    }

    return make_scene<IgniteExplosiveScene>(position());
}



void Explosive::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::dynamite;
}



void Explosive::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::dynamite;
}



static SharedVariable tnt_damage("dynamite_damage", 50);
static SharedVariable tnt_range("dynamite_range", 1);



void Explosive::apply_damage(Health damage, const DamageConfiguration& conf)
{
    Room::apply_damage(damage, conf);

    ignition_ = true;
}



void Explosive::ignite(int range, Health damage, bool spread_fire)
{
    make_flak_smoke(center());
    make_flak_smoke(center());

    for (int i = 0; i < 2; ++i) {
        if (auto e =
                alloc_entity<ExploTrail>(center(),
                                         rng::choice<360>(rng::utility_state),
                                         1.25_fixed,
                                         seconds(2))) {
            APP.effects().push(std::move(e));
        }
    }

    Vec2<s32> pos;
    pos.x = center().x.as_integer();
    pos.y = center().y.as_integer();

    APP.on_timeout(milliseconds(190), [pos]() {
        Vec2<Fixnum> p;
        p.x = Fixnum::from_integer(pos.x);
        p.y = Fixnum::from_integer(pos.y);
        make_flak_smoke(p);
    });

    auto targets =
        allocate_dynamic<Buffer<Room*, 300>>("dynamite-target-bufer");

    for (int x = -range; x < range + 1; ++x) {
        for (int y = -range; y < range + 1; ++y) {
            if (position().x + x < 0 or position().y + y < 0 or
                position().x + x > 15 or position().y + y > 15) {
                continue;
            }

            auto pos = RoomCoord{u8(position().x + x), u8(position().y + y)};

            if (auto room = parent()->get_room(pos)) {
                bool found = false;
                for (auto& target : *targets) {
                    if (target == room) {
                        found = true;
                        break;
                    }
                }
                if (not found) {
                    targets->push_back(room);
                }
            } else if (auto drone = parent()->get_drone(pos)) {
                (*drone)->apply_damage(damage);
            }
        }
    }

    for (auto& room : *targets) {

        if (room->cast<TNT>()) {
            auto d = clamp((int)damage, 0, (int)tnt_damage);
            room->apply_damage(std::min(room->health() + 1, d));
        } else {
            room->apply_damage(damage);
        }

        if (spread_fire and not((*room->metaclass())->properties() &
                                RoomProperties::fireproof)) {
            if (room->health() > 0) {
                room->parent()->fire_create(room->position());
            }
        }


        if (APP.game_mode() == App::GameMode::adventure or
            APP.game_mode() == App::GameMode::challenge or
            APP.game_mode() == App::GameMode::skyland_forever) {

            // Hack added for an achievement where you unlock dynamite-ii when
            // destroying invading goblins with dynamite.
            if (is_player_island(room->parent()) and room->health() == 0) {
                for (auto& chr : room->characters()) {
                    if (chr->owner() == &APP.opponent()) {
                        achievements::raise(achievements::Achievement::triage);
                        break;
                    }
                }
            }
        }
    }
}



void Explosive::finalize()
{
    Room::finalize();

    if (not ignition_) {
        return;
    } else {
        ignite(tnt_range, tnt_damage, false);
        ExploSpawner::create(center());
    }
}



void TNT::finalize()
{
    Room::finalize();

    if (not ignition_) {
        return;
    } else {
        ignite(2, 180, true);
        ExploSpawner::create(center());

        if (not PLATFORM.network_peer().is_connected()) {
            for (int i = 0; i < 10; ++i) {

                auto c = APP.alloc_entity<FireBolt>(
                    center(), center(), parent(), position());
                if (c) {
                    c->set_direction(rng::choice<359>(rng::critical_state));
                    parent()->projectiles().push(std::move(c));
                }
            }
        }
    }
}



static void display_damage_range_dist1(Platform::Screen& screen, Room& room)
{
    auto pos = room.position();

    auto origin = room.parent()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 1; x < pos.x + 2; ++x) {

        if (x == pos.x) {

            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);
            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y - 1) * 16)});
        } else {
            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_texture_index(13);
            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y - 1) * 16)});
        }

        screen.draw(sprite);

        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_tidx_16x16(13, 1);
        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y + 1) * 16)});
        screen.draw(sprite);
        sprite.set_texture_index(13);
    }
}



static void display_damage_range_dist2(Platform::Screen& screen, Room& room)
{
    auto pos = room.position();

    auto origin = room.parent()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y - 2) * 16)});
        screen.draw(sprite);


        if (x not_eq pos.x) {
            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);

            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y) * 16)});

            screen.draw(sprite);

            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_texture_index(13);
        }


        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y + 1) * 16)});

        screen.draw(sprite);
    }
}



void Explosive::display_on_hover(Platform::Screen& screen,

                                 const RoomCoord& cursor)
{
    display_damage_range_dist1(screen, *this);
}



void TNT::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::tnt;
}



void TNT::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::tnt;
}



void TNT::display_on_hover(Platform::Screen& screen,

                           const RoomCoord& cursor)
{
    display_damage_range_dist2(screen, *this);
}



void Cesium::update(Time delta)
{
    Room::update(delta);
    Room::ready();

    if (health() not_eq max_health()) {
        Room::ready();
        ignition_ = true;

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(10, {});
            damage_timer_ = 0;
        }

    } else {

        u8 x = position().x;
        u8 y = position().y;

        if (auto room = parent()->get_room({x, u8(y - 1)})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1, {});
                return;
            }
        }

        if (auto room = parent()->get_room({x, u8(y + 1)})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1, {});
                return;
            }
        }

        if (auto room = parent()->get_room({u8(x + 1), y})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1, {});
                return;
            }
        }

        if (auto room = parent()->get_room({u8(x - 1), y})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1, {});
                return;
            }
        }
    }
}



void Cesium::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::cesium;
}



void Cesium::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::cesium;
}



void Cesium::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_cesium)->c_str();
}



void Cesium::display_on_hover(Platform::Screen& screen,

                              const RoomCoord& cursor)
{
    display_damage_range_dist2(screen, *this);
}



} // namespace skyland
