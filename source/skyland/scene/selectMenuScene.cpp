#include "selectMenuScene.hpp"
#include "constructionScene.hpp"
#include "inspectP2Scene.hpp"
#include "modules/glossaryViewerModule.hpp"
#include "moveCharacterScene.hpp"
#include "moveRoomScene.hpp"
#include "readyScene.hpp"
#include "salvageRoomScene.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



Island* SelectMenuScene::island(App& app) const
{
    if (is_far_camera()) {
        return opponent_island(app);
    } else {
        return &player_island(app);
    }
}



static const auto highlight_colors =
    FontColors{custom_color(0x000010), ColorConstant::aerospace_orange};



void SelectMenuScene::redraw_line(Platform& pfrm, int line, bool highlight)
{
    opts_->lines_[line].assign(loadstr(pfrm, opts_->strings_[line])->c_str(),
                               highlight ? highlight_colors
                                         : Text::OptColors{});

    for (int i = opts_->lines_[line].len(); i < opts_->longest_line_; ++i) {
        opts_->lines_[line].append(
            " ", highlight ? highlight_colors : Text::OptColors{});
    }
}



void SelectMenuScene::enter(Platform& pfrm, App& app, Scene& scene)
{
    disable_ui();
    disable_gamespeed_icon();


    ActiveWorldScene::enter(pfrm, app, scene);

    if (auto ws = scene.cast_world_scene()) {
        if (ws->is_far_camera()) {
            far_camera();
        }
    }

    auto cursor = is_far_camera() ? globals().far_cursor_loc_
                                  : globals().near_cursor_loc_;


    pfrm.screen().clear();
    display(pfrm, app);
    pfrm.fill_overlay(0);
    pfrm.screen().display();

    auto add_line = [&](SystemString str, auto callback) {
        auto line = loadstr(pfrm, str);
        u8 y = opts_->lines_.size() + 1;
        opts_->lines_.emplace_back(pfrm, OverlayCoord{1, y});
        if (opts_->lines_.size() == 1) {
            opts_->lines_.back().assign(line->c_str(), highlight_colors);
        } else {
            opts_->lines_.back().assign(line->c_str());
        }
        opts_->longest_line_ =
            std::max(str_len(line->c_str()), u32(opts_->longest_line_));
        opts_->strings_.push_back(str);
        opts_->callbacks_.push_back(callback);
    };

    if (auto isle = island(app)) {
        if (isle == &app.player_island() or
            app.game_mode() == App::GameMode::sandbox) {
            if (isle->interior_visible()) {
                add_line(SystemString::sel_menu_view_exterior,
                         [](Platform& pfrm, App& app) -> ScenePtr<Scene> {
                             show_island_exterior(
                                 pfrm, app, &app.player_island());
                             return null_scene();
                         });
            } else {
                add_line(SystemString::sel_menu_view_interior,
                         [](Platform& pfrm, App& app) -> ScenePtr<Scene> {
                             show_island_interior(
                                 pfrm, app, &app.player_island());
                             return null_scene();
                         });
            }
        }

        if (not pfrm.network_peer().is_connected()) {
            if (isle == &app.player_island() or
                app.game_mode() == App::GameMode::sandbox) {
                add_line(SystemString::sel_menu_move_blocks,
                         [far = is_far_camera()](Platform& pfrm, App& app) {
                             return scene_pool::alloc<MoveRoomScene>(app,
                                                                     not far);
                         });
            }
        }

        if (auto chr = isle->character_at_location(cursor)) {
            add_line(
                SystemString::sel_menu_name_crewmember,
                [id = chr->id(),
                 far = is_far_camera(),
                 vis = isle->interior_visible()](Platform& pfrm, App& app) {
                    if (not vis) {
                        show_island_interior(pfrm, app, &app.player_island());
                    }

                    auto next =
                        scene_pool::alloc<ModifyCharacterScene>(id, not far);
                    next->modify_name_ = true;
                    return next;
                });

        } else if (auto room = isle->get_room(cursor)) {
            if ((isle == &app.player_island() or
                 app.game_mode() == App::GameMode::sandbox) and
                app.game_mode() not_eq App::GameMode::co_op) {
                if ((*room->metaclass())->category() ==
                    Room::Category::weapon) {

                    if (not pfrm.network_peer().is_connected()) {
                        add_line(SystemString::sel_menu_weapon_halt,
                                 [this, cursor](Platform& pfrm, App& app) {
                                     if (auto room =
                                             island(app)->get_room(cursor)) {
                                         room->unset_target(pfrm, app);
                                     }
                                     return null_scene();
                                 });
                    }
                }
            }
        }

        bool bird_found = false;
        for (auto& bird : app.birds()) {
            if (bird->island(app) == island(app) and
                bird->coordinate() == cursor) {
                bird_found = true;
            }
        }
        if (bird_found) {
            add_line(SystemString::sel_menu_spook_bird,
                     [this, cursor](Platform& pfrm, App& app) {
                         for (auto& bird : app.birds()) {
                             if (bird->island(app) == island(app) and
                                 bird->coordinate() == cursor) {
                                 pfrm.speaker().play_sound("seagull_1", 0);
                                 bird->signal(pfrm, app);
                             }
                         }
                         return null_scene();
                     });
        }


        if (not pfrm.network_peer().is_connected()) {
            auto room = island(app)->get_room(cursor);
            if (room and (room->description_visible() or
                          room->parent() == &app.player_island())) {
                add_line(
                    SystemString::sel_menu_describe_block,
                    [this, cursor](Platform& pfrm,
                                   App& app) -> ScenePtr<Scene> {
                        if (auto room = island(app)->get_room(cursor)) {
                            auto mt = room->metaclass_index();
                            auto next =
                                scene_pool::alloc<GlossaryViewerModule>(mt);
                            next->inspect_ = true;
                            next->skip_categories();
                            next->set_next_scene([&pfrm,
                                                  far = is_far_camera()]()
                                                     -> ScenePtr<Scene> {
                                pfrm.screen().schedule_fade(0);
                                if (far) {
                                    return scene_pool::alloc<InspectP2Scene>();
                                } else {
                                    return scene_pool::alloc<ReadyScene>();
                                }
                                return null_scene();
                            });
                            return next;
                        }
                        return null_scene();
                    });
            }
        }
    }

    add_line(SystemString::sel_menu_back,
             [this, cursor](Platform& pfrm, App& app) { return null_scene(); });

    for (int i = 0; i < opts_->longest_line_ + 1; ++i) {
        pfrm.set_tile(Layer::overlay, i, 0, 425);
    }

    for (auto& line : opts_->lines_) {
        for (int i = line.len(); i < opts_->longest_line_; ++i) {
            if (&line == opts_->lines_.begin()) {
                line.append(" ", highlight_colors);
            } else {
                line.append(" ");
            }
        }
    }
    for (u32 y = 0; y < opts_->lines_.size(); ++y) {
        pfrm.set_tile(Layer::overlay, 0, y + 1, 112);
    }
    pfrm.set_tile(Layer::overlay, 0, 1, 475);
}



void SelectMenuScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    pfrm.fill_overlay(0);
    pfrm.screen().clear();
    display(pfrm, app);
    pfrm.set_overlay_origin(0, 0);
    pfrm.screen().display();

    opts_->lines_.clear();
}



ScenePtr<Scene>
SelectMenuScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    for (u32 x = opts_->longest_line_ + 1; x < 30; ++x) {
        for (u32 y = 0; y < 20; ++y) {
            auto t = pfrm.get_tile(Layer::overlay, x, y);
            if (t) {
                pfrm.set_tile(Layer::overlay, x, y, 0);
            }
        }
    }

    if ((is_far_camera() and not app.opponent_island()) or
        player(app).key_down(pfrm, Key::action_2)) {
        if (is_far_camera()) {
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
        }
    }

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };


    if (test_key(Key::down)) {
        pfrm.speaker().play_sound("cursor_tick", 0);
        pfrm.set_tile(Layer::overlay, 0, sel_ + 1, 112);
        redraw_line(pfrm, sel_, false);
        if ((u32)sel_ < opts_->lines_.size() - 1) {
            ++sel_;
        } else {
            sel_ = 0;
        }
        pfrm.set_tile(Layer::overlay, 0, sel_ + 1, 475);
        redraw_line(pfrm, sel_, true);
    } else if (test_key(Key::up)) {
        pfrm.speaker().play_sound("cursor_tick", 0);
        pfrm.set_tile(Layer::overlay, 0, sel_ + 1, 112);
        redraw_line(pfrm, sel_, false);
        if (sel_ > 0) {
            --sel_;
        } else if (sel_ == 0) {
            sel_ = opts_->lines_.size() - 1;
        }
        pfrm.set_tile(Layer::overlay, 0, sel_ + 1, 475);
        redraw_line(pfrm, sel_, true);
    }

    if (player(app).key_down(pfrm, Key::action_1) or
        player(app).key_down(pfrm, Key::select)) {
        if (auto next = opts_->callbacks_[sel_](pfrm, app)) {
            return next;
        } else {
            if (is_far_camera()) {
                return scene_pool::alloc<InspectP2Scene>();
            } else {
                return scene_pool::alloc<ReadyScene>();
            }
        }
    }


    if (auto isle = island(app)) {
        auto cursor = is_far_camera() ? globals().far_cursor_loc_
                                      : globals().near_cursor_loc_;

        auto pos = isle->visual_origin();
        pos.x += Fixnum::from_integer(cursor.x * 16);
        pos.y += Fixnum::from_integer(cursor.y * 16);

        auto view_center = pfrm.screen().get_view().get_center();
        pos.x -= Fixnum(view_center.x);
        pos.y -= Fixnum(view_center.y) + 8.0_fixed;

        static const Fixnum offset = 8.0_fixed;

        // Right-align menu box if right-aligned box is partially offscreen.
        Fixnum box_width = Fixnum::from_integer((opts_->longest_line_ + 1) * 8);
        if (pos.x + box_width + offset > 240.0_fixed) {
            pos.x -= box_width - (16.0_fixed - offset);
        } else {
            pos.x += offset;
        }
        pos.y += 8.0_fixed;

        pfrm.set_overlay_origin(-pos.x.as_integer(), -pos.y.as_integer());
    }

    return null_scene();
}



void SelectMenuScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((97 * 2));

    auto origin = app.player_island().visual_origin();
    auto cursor_loc = globals().near_cursor_loc_;

    if (is_far_camera()) {
        if (app.opponent_island()) {
            origin = app.opponent_island()->visual_origin();
            cursor_loc = globals().far_cursor_loc_;
        }
    }

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);

    if (not is_far_camera()) {
        if (auto isle = island(app)) {
            if (auto room = isle->get_room(cursor_loc)) {
                room->display_on_hover(pfrm.screen(), app, cursor_loc);
            }
        }
    }

    WorldScene::display(pfrm, app);
}



} // namespace skyland
