#include "constructionScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "localization.hpp"
#include "platform/platform.hpp"
#include "readyScene.hpp"
#include "salvageRoomScene.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "worldScene.hpp"
#include "skyland/sound.hpp"



namespace skyland {



int construction_zone_min_y = 6;



Island* ConstructionScene::island(App& app)
{
    if (near_) {
        return &app.player_island();
    } else if (app.opponent_island()) {
        return &*app.opponent_island();
    } else {
        return nullptr;
    }
}



static Sound sound_openbag("openbag");



static Coins get_cost(Island* island, const RoomMeta& meta)
{
    Coins cost = meta->cost();
    for (int i = 0; i < island->workshop_count() + island->foundry_count();
         ++i) {
        cost *= 0.9f;
    }
    if (cost < meta->cost() * salvage_factor) {
        // You shouldn't be able to farm coins by building a bunch of workshops
        // and salvaging rooms repeatedly.
        return meta->cost() * salvage_factor;
    }
    return cost;
}



static bool show_construction_icons = true;



ScenePtr<Scene>
ConstructionScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto exit_scene = [this]() -> ScenePtr<Scene> {
        if (near_) {
            return scene_pool::alloc<ReadyScene>();
        } else {
            return scene_pool::alloc<InspectP2Scene>();
        }
    };

    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (not island(app)) {
        return exit_scene();
    }

    auto& cursor_loc =
        near_ ? std::get<SkylandGlobalData>(globals()).near_cursor_loc_
              : std::get<SkylandGlobalData>(globals()).far_cursor_loc_;


    if (app.player().key_down(pfrm, Key::alt_2) or
        (state_ == State::select_loc and
         app.player().key_down(pfrm, Key::action_2))) {
        if (not construction_sites_.empty()) {
            cursor_loc.x = construction_sites_[selector_].x;
            cursor_loc.y = construction_sites_[selector_].y;
        }
        return exit_scene();
    }

    switch (state_) {
    case State::select_loc:
        if (app.player().key_down(pfrm, Key::right)) {
            if (selector_ < construction_sites_.size() - 1) {
                ++selector_;
            } else if (near_ and app.game_mode() == App::GameMode::sandbox) {
                auto& cursor_loc =
                    std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

                cursor_loc.x = 0;
                cursor_loc.y =
                    std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y;
                return scene_pool::alloc<ConstructionScene>(false);
            }
        }

        if (app.player().key_down(pfrm, Key::left)) {
            if (selector_ > 0) {
                --selector_;
            } else if (not near_ and
                       app.game_mode() == App::GameMode::sandbox) {
                auto& cursor_loc =
                    std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

                cursor_loc.x = app.player_island().terrain().size();
                cursor_loc.y =
                    std::get<SkylandGlobalData>(globals()).far_cursor_loc_.y;
                return scene_pool::alloc<ConstructionScene>(true);
            }
        }

        if (not construction_sites_.empty()) {
            cursor_loc.x = construction_sites_[selector_].x;
            cursor_loc.y = construction_sites_[selector_].y;
        }

        if (app.player().key_down(pfrm, Key::action_1) and
            not construction_sites_.empty()) {

            if (construction_sites_[selector_].y == 15) {
                // Special case: we want to add to the terrain level, not
                // construct a building.
                state_ = State::add_terrain;
                StringBuffer<30> temp(":build :add-terrain ");
                temp += to_string<10>(app.terrain_cost());
                temp += "@";
                msg(pfrm, temp.c_str());

            } else {
                collect_available_buildings(pfrm, app);

                if (not available_buildings_.empty()) {

                    if (last_constructed_building_ and
                        (available_buildings_[building_selector_] not_eq
                         last_constructed_building_)) {

                        // Ok, so if we constructed a building, and the cursor
                        // advanced into a narrower slot, we may have fewer
                        // options for stuff to build. i.e. the
                        // available_buildings array was effectively
                        // re-indexed. If our former selection still exists in
                        // the list, adjust the selector accordingly. This isn't
                        // strictly necessary, it just makes the UI feel nicer to
                        // the player.

                        for (u32 i = 0; i < available_buildings_.size(); ++i) {
                            if (available_buildings_[i] ==
                                last_constructed_building_) {
                                building_selector_ = i;
                                break;
                            }
                        }
                    }

                    state_ = State::choose_building;
                    sound_openbag.play(pfrm, 1, seconds(2));
                    sound_openbag.reset();
                    show_current_building_text(pfrm, app);
                }
            }
        }
        break;

    case State::choose_building:
        if (app.player().key_down(pfrm, Key::action_2)) {
            find_construction_sites(pfrm, app);
            state_ = State::select_loc;
            msg(pfrm, ":build");
            break;
        }

        if (app.player().key_down(pfrm, Key::start)) {
            show_construction_icons = not show_construction_icons;
            show_current_building_text(pfrm, app);
        }

        if (app.player().key_down(pfrm, Key::down)) {
            building_selector_ += 5;
            building_selector_ %= available_buildings_.size();
            show_current_building_text(pfrm, app);
        }

        if (app.player().key_down(pfrm, Key::up)) {
            building_selector_ -= 5;
            building_selector_ %= available_buildings_.size();
            show_current_building_text(pfrm, app);
        }

        if (app.player().key_down(pfrm, Key::right)) {
            pfrm.speaker().play_sound("click", 1);
            if (building_selector_ < (int)available_buildings_.size() - 1) {
                ++building_selector_;
                show_current_building_text(pfrm, app);
            } else {
                building_selector_ = 0;
                show_current_building_text(pfrm, app);
            }
        }

        if (app.player().key_down(pfrm, Key::left)) {
            pfrm.speaker().play_sound("click", 1);
            if (building_selector_ > 0) {
                --building_selector_;
                show_current_building_text(pfrm, app);
            } else {
                building_selector_ = available_buildings_.size() - 1;
                show_current_building_text(pfrm, app);
            }
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            const auto& target = *available_buildings_[building_selector_];

            if (app.coins() < get_cost(island(app), target)) {
                msg(pfrm, "insufficient funds!");
                state_ = State::insufficient_funds;
                break;
            }

            if (island(app)->power_supply() - island(app)->power_drain() <
                target->consumes_power()) {
                msg(pfrm, "insufficient power supply!");
                state_ = State::insufficient_funds;
                break;
            }

            if (room_pool::pool_->empty() or island(app)->rooms().full()) {
                msg(pfrm, "too many rooms");
                state_ = State::insufficient_funds;
                break;
            }

            const auto diff = get_cost(island(app), target);
            app.coins() -= diff;
            app.level_coins_spent() += diff;

            const auto sz = target->size().y;
            const u8 dest_x = construction_sites_[selector_].x;
            const u8 dest_y = construction_sites_[selector_].y - (sz - 1);

            pfrm.speaker().play_sound("build0", 4);

            target->create(pfrm, app, island(app), {dest_x, dest_y});
            last_constructed_building_ = &target;

            app.player().rooms_built_++;

            network::packet::RoomConstructed packet;
            packet.metaclass_index_.set(metaclass_index(target->name()));
            packet.x_ = dest_x;
            packet.y_ = dest_y;
            network::transmit(pfrm, packet);

            find_construction_sites(pfrm, app);

            state_ = State::select_loc;
            msg(pfrm, ":build");
        }
        break;

    case State::insufficient_funds:
        if (app.player().key_down(pfrm, Key::action_2) or
            app.player().key_down(pfrm, Key::action_1)) {
            find_construction_sites(pfrm, app);
            state_ = State::select_loc;
            msg(pfrm, ":build");
        }
        break;

    case State::add_terrain:
        if (app.player().key_down(pfrm, Key::action_2)) {
            find_construction_sites(pfrm, app);
            state_ = State::select_loc;
            msg(pfrm, ":build");
            break;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            if (app.coins() < app.terrain_cost()) {
                msg(pfrm, "insufficient funds!");
                state_ = State::insufficient_funds;
                break;
            }

            app.coins() -= app.terrain_cost();

            auto& terrain = island(app)->terrain();
            terrain.pop_back(); // the old edge tile
            terrain.push_back(Tile::terrain_middle);
            terrain.push_back(Tile::terrain_right);

            pfrm.speaker().play_sound("gravel", 4);

            island(app)->render_terrain(pfrm);

            network::packet::TerrainConstructed packet;
            packet.new_terrain_size_ = island(app)->terrain().size();
            network::transmit(pfrm, packet);


            find_construction_sites(pfrm, app);
            state_ = State::select_loc;

            msg(pfrm, ":build");
        }
        break;
    }

    return null_scene();
}


void ConstructionScene::show_current_building_text(Platform& pfrm, App& app)
{
    StringBuffer<32> str = ":build :";

    str += (*available_buildings_[building_selector_])->name();
    str += " ";
    str += to_string<10>(
        get_cost(island(app), (*available_buildings_[building_selector_])));
    str += "@";
    str += " ";
    str += to_string<10>(
        (*available_buildings_[building_selector_])->consumes_power());
    str += "`";

    msg(pfrm, str.c_str());


    auto st = calc_screen_tiles(pfrm);

    if (not show_construction_icons) {
        return;
    }

    for (int i = st.x - 25; i < st.x - 5; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 6, 425);
    }

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        pfrm.set_tile(Layer::overlay, st.x - 26, y, 128);
        pfrm.set_tile(Layer::overlay, st.x - 5, y, 433);
    }

    pfrm.set_tile(Layer::overlay, st.x - 26, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 5, st.y - 2, 418);

    {
        int index = building_selector_;
        if (index - 2 < -1) {
            index = available_buildings_.size() - 2;
        } else if (index - 2 < 0) {
            index = available_buildings_.size() - 1;
        } else {
            index = index - 2;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = building_selector_;
        if (index - 1 < 0) {
            index = available_buildings_.size() - 1;
        } else {
            index = index - 1;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon = (*available_buildings_[building_selector_])->icon();
        draw_image(pfrm, 197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = building_selector_;
        if (index + 1 >= (int)available_buildings_.size()) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(213, icon, 16);
    }

    {
        int index = building_selector_;
        if (index + 1 >= (int)available_buildings_.size()) {
            index = 1;
        } else if (index + 2 >= (int)available_buildings_.size()) {
            index = 0;
        } else {
            index = index + 2;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(274, icon, 16);
    }
}



void ConstructionScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (not island(app)) {
        return;
    }

    switch (state_) {
    case State::insufficient_funds:
        break;

    case State::select_loc:
        if (not construction_sites_.empty()) {
            auto origin = island(app)->visual_origin();

            origin.x += construction_sites_[selector_].x * 16;
            origin.y += (construction_sites_[selector_].y) * 16;

            Sprite sprite;
            sprite.set_position(origin);

            if (construction_sites_[selector_].y == 15) {
                // Display a different icon when constructing terrain, as a hint
                // to the player that he/she can expand an island's terrain.
                sprite.set_texture_index(73);
            } else {
                sprite.set_texture_index(12);
            }

            sprite.set_size(Sprite::Size::w16_h32);


            pfrm.screen().draw(sprite);
        }
        break;


    case State::choose_building:
        if (not available_buildings_.empty()) {
            const auto& meta = *available_buildings_[building_selector_];
            const auto sz = meta->size();

            auto origin = island(app)->visual_origin();
            origin.x += construction_sites_[selector_].x * 16;
            origin.y += (construction_sites_[selector_].y - (sz.y - 1)) * 16;

            if (sz.x == 1 and sz.y == 1) {
                Sprite sprite;
                sprite.set_texture_index(14);
                sprite.set_size(Sprite::Size::w16_h32);
                sprite.set_position({origin.x, origin.y - 16});
                pfrm.screen().draw(sprite);
            } else if (sz.x == 2 and sz.y == 1) {
                Sprite sprite;
                sprite.set_texture_index(14);
                sprite.set_size(Sprite::Size::w16_h32);
                sprite.set_position({origin.x, origin.y - 16});
                pfrm.screen().draw(sprite);
                sprite.set_position({origin.x + 16, origin.y - 16});
                pfrm.screen().draw(sprite);
            } else {
                Sprite sprite;
                sprite.set_texture_index(13);
                sprite.set_size(Sprite::Size::w16_h32);

                for (int x = 0; x < sz.x; ++x) {
                    for (int y = 0; y < sz.y / 2; ++y) {
                        sprite.set_position(
                            {origin.x + x * 16, origin.y + y * 32});
                        pfrm.screen().draw(sprite);
                    }
                }
            }
        }
        break;


    case State::add_terrain: {
        auto& terrain = island(app)->terrain();
        const Vec2<u8> loc = {u8(terrain.size()), 15};
        auto origin = island(app)->visual_origin();
        origin.x += loc.x * 16;
        origin.y -= 32;
        Sprite sprite;
        sprite.set_texture_index(14);
        sprite.set_size(Sprite::Size::w16_h32);
        sprite.set_position(origin);
        pfrm.screen().draw(sprite);
        break;
    }
    }
}



void ConstructionScene::find_construction_sites(Platform& pfrm, App& app)
{
    construction_sites_.clear();

    bool matrix[16][16];

    island(app)->plot_construction_zones(matrix);

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y] and y > construction_zone_min_y) {
                construction_sites_.push_back({x, y});
            }
        }
    }

    auto& terrain = island(app)->terrain();
    if (not terrain.full() and not pfrm.network_peer().is_connected()) {
        construction_sites_.push_back({u8(terrain.size()), 15});
    }

    if (construction_sites_.empty()) {
        selector_ = 0;
    } else if (selector_ >= construction_sites_.size()) {
        selector_--;
    }
}



void ConstructionScene::msg(Platform& pfrm, const char* text)
{
    auto st = calc_screen_tiles(pfrm);
    text_.emplace(pfrm, text, OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
        pfrm.set_tile(Layer::overlay, i, st.y - 3, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 5, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 6, 0);
    }
}



void ConstructionScene::collect_available_buildings(Platform& pfrm, App& app)
{
    available_buildings_.clear();

    int avail_space = 1;

    const auto current = construction_sites_[selector_];
    for (auto& site : construction_sites_) {
        if (site.y == current.y and site.x == current.x + 1) {
            // FIXME: buildings wider than 2, various other cases
            avail_space = 2;
        }
    }

    const int avail_y_space = current.y - construction_zone_min_y;

    const auto w_count =
        island(app)->workshop_count() + island(app)->foundry_count();

    const auto f_count = island(app)->foundry_count();

    auto metatable = room_metatable();
    for (int i = 0; i < metatable.second; ++i) {
        auto& meta = metatable.first[i];

        const bool workshop_required =
            (meta->conditions() & Conditions::workshop_required);

        const bool foundry_required =
            (meta->conditions() & Conditions::foundry_required);

        if (meta->size().x <= avail_space and
            meta->size().y <= avail_y_space and
            (not foundry_required or (foundry_required and f_count > 0)) and
            (not workshop_required or (workshop_required and w_count > 0)) and
            not(meta->conditions() & Conditions::not_constructible)) {
            available_buildings_.push_back(&meta);
        }
    }

    if (building_selector_ >= (int)available_buildings_.size()) {
        building_selector_ = available_buildings_.size() - 1;
    }
}



void ConstructionScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (not near_) {
        power_fraction_opponent_island_ = true;
    }

    // if (not dynamic_cast<ConstructionScene*>(&prev)) {
    //     pfrm.speaker().play_sound("openbag", 1);
    // }

    WorldScene::enter(pfrm, app, prev);

    if (not near_) {
        far_camera();
    }

    persist_ui();

    find_construction_sites(pfrm, app);

    if (not construction_sites_.empty()) {
        auto& cursor_loc =
            near_ ? std::get<SkylandGlobalData>(globals()).near_cursor_loc_
                  : std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

        for (u32 i = 0; i < construction_sites_.size(); ++i) {
            if (construction_sites_[i].x == cursor_loc.x) {

                selector_ = i;
            }
        }
    }

    msg(pfrm, ":build");
}



void ConstructionScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    if (dynamic_cast<ConstructionScene*>(&next)) {
        // We do not want the menus to flicker between scenes when we switch
        // between two construction scenes, so disable cleanup for text. Only
        // really happens in sandbox mode, where you can build on either your
        // own castle or the opponent's.
        text_->__detach();
    } else {
        text_.reset();
        pfrm.fill_overlay(0);
    }
}



} // namespace skyland
