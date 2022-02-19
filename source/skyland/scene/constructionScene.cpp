#include "constructionScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "modules/glossaryViewerModule.hpp"
#include "platform/platform.hpp"
#include "readyScene.hpp"
#include "salvageRoomScene.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "worldScene.hpp"



namespace skyland {



const int construction_zone_min_y = 5;



u16 room_category_icon(Room::Category category)
{
    return 379 + static_cast<u16>(category);
}



Island* ConstructionScene::island(App& app)
{
    if (near_) {
        return &app.player_island();
    } else if (app.opponent_island()) {
        return app.opponent_island();
    } else {
        return nullptr;
    }
}



static Sound sound_openbag("openbag");



static Coins get_cost(Island* island, const RoomMeta& meta)
{
    Coins cost = meta->cost();
    for (int i = 0; i < island->workshop_count() + island->manufactory_count();
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



bool tapped_topleft_corner(Platform& pfrm, App& app);



std::optional<Vec2<u8>>
get_local_tapclick(Platform& pfrm, Island* island, const Vec2<u32>& pos)
{
    const auto view_offset = pfrm.screen().get_view().get_center().cast<s32>();

    auto island_pos = island->get_position();
    island_pos.x -= view_offset.x;
    island_pos.y -= view_offset.y;

    if (pos.x >= island_pos.x and
        pos.x <= island_pos.x + (1 + island->terrain().size()) * 16) {

        int x_tile = -((island_pos.x - pos.x) / 16);
        int y_tile = -((island_pos.y - pos.y) / 16);

        y_tile += 31; // FIXME!

        return {{(u8)x_tile, (u8)y_tile}};
    }

    return {};
}



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

    if (app.player().key_down(pfrm, Key::start)) {
        auto next = scene_pool::alloc<GlossaryViewerModule>();
        if (next) {
            const bool near = near_;
            next->set_next_scene([near]() {
                return scene_pool::alloc<ConstructionScene>(near);
            });
            return next;
        }
    }

    if (not island(app)) {
        return exit_scene();
    }

    auto& cursor_loc =
        near_ ? std::get<SkylandGlobalData>(globals()).near_cursor_loc_
              : std::get<SkylandGlobalData>(globals()).far_cursor_loc_;


    if (tapped_topleft_corner(pfrm, app) or
        app.player().key_down(pfrm, Key::alt_2) or
        (state_ == State::select_loc and
         app.player().key_down(pfrm, Key::action_2))) {
        if (not construction_sites_.empty()) {
            cursor_loc.x = construction_sites_[selector_].x;
            cursor_loc.y = construction_sites_[selector_].y;
        }
        return exit_scene();
    }


    auto tapclick = [&]() -> std::optional<Vec2<u8>> {
        if (auto pos = app.player().tap_released(pfrm)) {
            auto clk = get_local_tapclick(pfrm, island(app), *pos);

            return clk;
        }
        return std::nullopt;
    }();


    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };


    switch (state_) {
    case State::select_loc:

        if (test_key(Key::right)) {
            if (selector_ < construction_sites_.size() - 1) {
                ++selector_;
            } else if (near_ and app.game_mode() == App::GameMode::sandbox and
                       app.opponent_island()) {
                auto& cursor_loc =
                    std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

                cursor_loc.x = 0;
                cursor_loc.y =
                    std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y;
                return scene_pool::alloc<ConstructionScene>(false);
            }
        }

        if (test_key(Key::left)) {
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

        if (((tapclick and *tapclick == construction_sites_[selector_]) or
             app.player().key_down(pfrm, Key::action_1)) and
            not construction_sites_.empty()) {

            tapclick.reset();

            if (construction_sites_[selector_].y == 15) {
                // Special case: we want to add to the terrain level, not
                // construct a building.
                state_ = State::add_terrain;
                StringBuffer<30> temp;
                temp += *SYSTR(construction_add_terrain);
                temp += stringify(app.terrain_cost());
                temp += "@";
                msg(pfrm, temp.c_str());

            } else {
                collect_available_buildings(pfrm, app);

                if (not available_buildings_.empty()) {

                    if (last_constructed_building_ and
                        (available_buildings_[building_selector_] not_eq
                         *last_constructed_building_)) {

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
                                *last_constructed_building_) {
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
        } else if (tapclick or app.player().touch_held(milliseconds(200))) {

            if (app.player().touch_held(milliseconds(200))) {
                // If the player presses and holds the touch screen, scroll
                // through available construction sites.
                if (auto pos = app.player().touch_current(pfrm)) {
                    tapclick = get_local_tapclick(pfrm, island(app), *pos);
                }
            }

            if (tapclick) {
                // First try to find an exact match. If not, try a loose match
                // based on x coordinate.
                for (u32 i = 0; i < construction_sites_.size(); ++i) {
                    if (construction_sites_[i] == *tapclick) {
                        tapclick.reset();
                        selector_ = i;
                        camera_update_timer_ = milliseconds(500);
                        break;
                    }
                }
                if (tapclick) {
                    for (u32 i = 0; i < construction_sites_.size(); ++i) {
                        if (construction_sites_[i].x == tapclick->x) {
                            tapclick.reset();
                            selector_ = i;
                            camera_update_timer_ = milliseconds(500);
                            break;
                        }
                    }
                }
            }
        }
        break;

    case State::choose_building: {
        auto scroll_right = [&] {
            pfrm.speaker().play_sound("click", 1);
            if (building_selector_ < (int)available_buildings_.size() - 1) {
                ++building_selector_;
                show_current_building_text(pfrm, app);
            } else {
                building_selector_ = 0;
                show_current_building_text(pfrm, app);
            }
        };
        auto scroll_left = [&] {
            pfrm.speaker().play_sound("click", 1);
            if (building_selector_ > 0) {
                --building_selector_;
                show_current_building_text(pfrm, app);
            } else {
                building_selector_ = available_buildings_.size() - 1;
                show_current_building_text(pfrm, app);
            }
        };
        if (app.player().touch_held(milliseconds(200))) {
            if (auto p = app.player().touch_current(pfrm)) {
                if (last_touch_x_) {
                    touchscroll_ += p->x - last_touch_x_;
                    last_touch_x_ = p->x;
                } else {
                    last_touch_x_ = p->x;
                }
            }
            if (touchscroll_ < -16) {
                touchscroll_ = 0;
                scroll_right();
            } else if (touchscroll_ > 16) {
                touchscroll_ = 0;
                scroll_left();
            }
        } else if (app.player().key_down(pfrm, Key::action_2) or
                   (tapclick and
                    *tapclick not_eq construction_sites_[selector_])) {
            find_construction_sites(pfrm, app);
            state_ = State::select_loc;
            msg(pfrm, SYSTR(construction_build)->c_str());
            last_touch_x_ = 0;
            break;
        } else {
            last_touch_x_ = 0;
        }

        if (app.player().key_down(pfrm, Key::down)) {
            pfrm.speaker().play_sound("click", 1);
            auto current_category =
                (*load_metaclass(available_buildings_[building_selector_]))
                    ->category();
            // When the player presses down, jump to the next building in the
            // list of a different category. The room metatable should be
            // organized by room category by convention.
            u32 i = 0;
            for (i = 0; i < available_buildings_.size(); ++i) {
                auto other_category =
                    (*load_metaclass(
                         available_buildings_[(building_selector_ + i) %
                                              available_buildings_.size()]))
                        ->category();
                if (other_category not_eq current_category) {
                    break;
                }
            }
            building_selector_ += i;
            building_selector_ %= available_buildings_.size();
            show_current_building_text(pfrm, app);
        }

        // FIXME: this code doesn't work!
        // if (app.player().key_down(pfrm, Key::up)) {
        //     pfrm.speaker().play_sound("click", 1);
        //     const auto current_category =
        //         (*load_metaclass(available_buildings_[building_selector_]))
        //             ->category();

        //     auto target_category = (Room::Category)(
        //         ((u32)current_category - 1) % (u32)Room::Category::count);

        //     if (target_category == current_category) {
        //         target_category =
        //             (Room::Category)((int)Room::Category::count - 1);
        //     }

        //     u32 i;
        //     for (i = 0; i < available_buildings_.size(); ++i) {
        //         if ((*load_metaclass(available_buildings_[i]))->category() ==
        //             target_category) {
        //             break;
        //         }
        //     }
        //     if (i >= available_buildings_.size()) {
        //         i = 0;
        //     }

        //     building_selector_ = i;
        //     show_current_building_text(pfrm, app);
        // }

        if (test_key(Key::right)) {
            scroll_right();
        }

        if (test_key(Key::left)) {
            scroll_left();
        }

        if (tapclick == construction_sites_[selector_] or
            app.player().key_down(pfrm, Key::action_1)) {
            const auto& target =
                *load_metaclass(available_buildings_[building_selector_]);

            if (app.coins() < get_cost(island(app), target)) {
                msg(pfrm, SYSTR(construction_insufficient_funds)->c_str());
                state_ = State::insufficient_funds;
                break;
            }

            if (island(app)->power_supply() - island(app)->power_drain() <
                target->consumes_power()) {
                msg(pfrm, SYSTR(construction_insufficient_power_supply)->c_str());
                state_ = State::insufficient_funds;
                break;
            }

            if (std::get<SkylandGlobalData>(globals()).room_pools_.empty() or
                island(app)->rooms().full()) {
                msg(pfrm, SYSTR(construction_too_many_rooms)->c_str());
                state_ = State::insufficient_funds;
                break;
            }

            const auto diff = get_cost(island(app), target);
            app.set_coins(pfrm, app.coins() - diff);
            app.level_coins_spent() += diff;

            const auto sz = target->size().y;
            const u8 dest_x = construction_sites_[selector_].x;
            const u8 dest_y = construction_sites_[selector_].y - (sz - 1);

            pfrm.speaker().play_sound("build0", 4);

            target->create(pfrm, app, island(app), {dest_x, dest_y});
            last_constructed_building_ = metaclass_index(target->name());

            app.player().rooms_built_++;

            auto mt_index = metaclass_index(target->name());

            network::packet::RoomConstructed packet;
            packet.metaclass_index_.set(mt_index);
            packet.x_ = dest_x;
            packet.y_ = dest_y;
            network::transmit(pfrm, packet);


            if (near_) {
                time_stream::event::PlayerRoomCreated p;
                p.x_ = dest_x;
                p.y_ = dest_y;
                app.time_stream().push(pfrm, app.level_timer(), p);
            } else {
                time_stream::event::OpponentRoomCreated p;
                p.x_ = dest_x;
                p.y_ = dest_y;
                app.time_stream().push(pfrm, app.level_timer(), p);
            }


            if (auto room = island(app)->get_room({dest_x, dest_y})) {
                if (auto scene = room->setup(pfrm, app)) {
                    return scene;
                }
            }


            find_construction_sites(pfrm, app);

            state_ = State::select_loc;
            msg(pfrm, SYSTR(construction_build)->c_str());
        }
        break;
    }

    case State::insufficient_funds:
        if (app.player().key_down(pfrm, Key::action_2) or
            app.player().key_down(pfrm, Key::action_1)) {
            find_construction_sites(pfrm, app);
            state_ = State::select_loc;
            msg(pfrm, SYSTR(construction_build)->c_str());
        }
        break;

    case State::add_terrain:
        if (app.player().key_down(pfrm, Key::action_2)) {
            find_construction_sites(pfrm, app);
            state_ = State::select_loc;
            msg(pfrm, SYSTR(construction_build)->c_str());
            break;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            if (app.coins() < app.terrain_cost()) {
                msg(pfrm, SYSTR(construction_build)->c_str());
                state_ = State::insufficient_funds;
                break;
            }

            app.set_coins(pfrm, app.coins() - app.terrain_cost());

            time_stream::event::IslandTerrainChanged e;
            e.previous_terrain_size_ = island(app)->terrain().size();
            e.near_ = island(app) == &app.player_island();
            app.time_stream().push(pfrm, app.level_timer(), e);


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

            msg(pfrm, SYSTR(construction_build)->c_str());
        }
        break;
    }

    return null_scene();
}


void ConstructionScene::show_current_building_text(Platform& pfrm, App& app)
{
    StringBuffer<32> str = SYSTR(construction_build)->c_str();
    str += " :";

    str += (*load_metaclass(available_buildings_[building_selector_]))
        ->ui_name(pfrm)->c_str();

    str += " ";
    str += stringify(
        get_cost(island(app),
                 (*load_metaclass(available_buildings_[building_selector_]))));
    str += "@";
    str += " ";
    str += stringify((*load_metaclass(available_buildings_[building_selector_]))
                         ->consumes_power());
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

        auto icon =
            (*load_metaclass(available_buildings_[index]))->unsel_icon();
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

        auto icon =
            (*load_metaclass(available_buildings_[index]))->unsel_icon();
        draw_image(pfrm, 181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon =
            (*load_metaclass(available_buildings_[building_selector_]))->icon();
        draw_image(pfrm, 197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        // Not sure whether I like the visual representation of the item
        // category. I might add it back at some point.
        // pfrm.set_tile(Layer::overlay, 29, 17,
        //               room_category_icon((*load_metaclass(available_buildings_[building_selector_]))->category()));

        // pfrm.set_tile(Layer::overlay, 29, 15, 425);
        // pfrm.set_tile(Layer::overlay, 29, 16, 398);
        // pfrm.set_tile(Layer::overlay, 29, 18, 399);

        // pfrm.set_tile(Layer::overlay, 28, 16, 128);
        // pfrm.set_tile(Layer::overlay, 28, 17, 128);
        // pfrm.set_tile(Layer::overlay, 28, 18, 419);



        pfrm.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = building_selector_;
        if (index + 1 >= (int)available_buildings_.size()) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon =
            (*load_metaclass(available_buildings_[index]))->unsel_icon();
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

        auto icon =
            (*load_metaclass(available_buildings_[index]))->unsel_icon();
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
            const auto& meta =
                *load_metaclass(available_buildings_[building_selector_]);
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

                if (sz.y % 2 not_eq 0) {
                    // Odd sized room in y direction. Draw bottom row:
                    sprite.set_texture_index(14);
                    for (int x = 0; x < sz.x; ++x) {
                        sprite.set_position(
                            {origin.x + x * 16, origin.y + (sz.y - 2) * 16});
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

    u8 matrix[16][16];
    island(app)->plot_rooms(matrix);

    int avail_x_space = 0;

    const auto current = construction_sites_[selector_];


    // Avail y space per x coordinate of the room that we're constructing.
    u8 avail_y_space[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    for (int x = current.x; x < (int)island(app)->terrain().size(); ++x) {
        if (matrix[x][current.y]) {
            break;
        }
        for (int y = current.y; y > 1; --y) {
            if (matrix[x][y]) {
                break;
            }
            ++avail_y_space[x - current.x];
        }
        ++avail_x_space;
    }

    if (avail_x_space == 0) {
        // We should have started with a construction site known to be valid, so
        // the minimum space available should 1,1.
        Platform::fatal("collect_avail_buildings(): logic error");
    }


    auto calc_avail_y_space = [&](int room_width) {
        int min = avail_y_space[0];
        for (int x = 0; x < room_width; ++x) {
            if (avail_y_space[x] < min) {
                min = avail_y_space[x];
            }
        }
        return min;
    };


    const auto w_count =
        island(app)->workshop_count() + island(app)->manufactory_count();

    const auto f_count = island(app)->manufactory_count();

    auto metatable = room_metatable();
    for (MetaclassIndex i = 0; i < metatable.second; ++i) {
        auto& meta = metatable.first[i];

        if (app.game_mode() == App::GameMode::multiplayer or
            app.game_mode() == App::GameMode::tutorial) {
            if (i >= plugin_rooms_begin()) {
                // We disable plugin (dlc) rooms during certain game modes.
                break;
            }
        }

        if (not is_enabled(i)) {
            continue;
        }

        const bool workshop_required =
            (meta->properties() & RoomProperties::workshop_required);

        const bool manufactory_required =
            (meta->properties() & RoomProperties::manufactory_required);

        const bool dependencies_satisfied =
            (not manufactory_required or
             (manufactory_required and f_count > 0) or
             app.game_mode() == App::GameMode::sandbox) and
            (not workshop_required or (workshop_required and w_count > 0) or
             app.game_mode() == App::GameMode::sandbox);

        const bool explicitly_disabled =
            (app.game_mode() == App::GameMode::tutorial and
             meta->properties() & RoomProperties::disabled_in_tutorials) or
            (meta->properties() & RoomProperties::not_constructible);


        if (not explicitly_disabled and meta->size().x <= avail_x_space and
            meta->size().y <= calc_avail_y_space(meta->size().x) and
            dependencies_satisfied) {

            // Do not show decorations in the building list in tutorial mode.
            if (not(app.game_mode() == App::GameMode::tutorial and
                    meta->category() == Room::Category::decoration)) {

                auto index = metaclass_index(meta->name());
                if (not available_buildings_.push_back(index)) {
                    Platform::fatal("TODO: available buildings buffer "
                                    "needs more memory");
                }
            }
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

    pfrm.screen().fade(0.f);

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

    msg(pfrm, SYSTR(construction_build)->c_str());
}



void ConstructionScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    if (dynamic_cast<ConstructionScene*>(&next)) {
        // We do not want the menus to flicker between scenes when we switch
        // between two construction scenes, so disable cleanup for text. Only
        // really happens in sandbox mode, where you can build on either your
        // own castle or the opponent's. dynamic_cast<> because it's such an
        // obscure edge case and I don't have time to fix it correctly right
        // now.
        text_->__detach();
    } else {
        text_.reset();
        pfrm.fill_overlay(0);
    }
}



} // namespace skyland
