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


#include "alloc_entity.hpp"
#include "allocator.hpp"
#include "configure_island.hpp"
#include "dataCart.hpp"
#include "eternal/eternal.hpp"
#include "heap_data.hpp"
#include "macrocosmEngine.hpp"
#include "platform/flash_filesystem.hpp"
#include "player/autopilotPlayer.hpp"
#include "player/opponent/enemyAI.hpp"
#include "player/opponent/friendlyAI.hpp"
#include "player/opponent/procgenEnemyAI.hpp"
#include "room_metatable.hpp"
#include "rooms/cargoBay.hpp"
#include "rooms/core.hpp"
#include "rooms/qrBlock.hpp"
#include "scene/constructionScene.hpp"
#include "scene/qrViewerScene.hpp"
#include "scene/readyScene.hpp"
#include "scene/scriptHookScene.hpp"
#include "scene/scriptedMenuScene.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "serial.hpp"
#include "sharedVariable.hpp"
#include "skyland.hpp"
#include "skyland/rooms/weapon.hpp"
#include "skyland/scene/itemShopScene.hpp"
#include "skyland/scene/lispReplScene.hpp"
#include "skyland/scene/modules/glossaryViewerModule.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"
#include "version.hpp"



#if not MAPBOX_ETERNAL_IS_CONSTEXPR
#error "NON-Constexpr lookup table!"
#endif



namespace lisp
{
Platform* interp_get_pfrm();
}



namespace skyland
{



App* __app = nullptr;


static App* interp_get_app()
{
    return __app;
}



using Syscall = lisp::Value* (*)(int);



MAPBOX_ETERNAL_CONSTEXPR const auto syscall_table =
    mapbox::eternal::map<mapbox::eternal::string, Syscall>(
        {{"setvar",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, string);
              L_EXPECT_OP(0, integer);

              if (auto v =
                      SharedVariable::load(lisp::get_op(1)->string().value())) {
                  v->set(lisp::get_op(0)->integer().value_);
                  return L_NIL;
              }

              StringBuffer<96> error("access to invalid shared variable '");
              error += lisp::get_op(1)->string().value();
              error += "'";

              Platform::fatal(error.c_str());
          }},
         {"getvar",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);
              L_EXPECT_OP(0, string);

              if (auto v =
                      SharedVariable::load(lisp::get_op(0)->string().value())) {
                  return lisp::make_integer(v->get());
              }

              StringBuffer<96> error("access to invalid shared variable '");
              error += lisp::get_op(0)->string().value();
              error += "'";

              Platform::fatal(error.c_str());
          }},
         {"save-bit-load",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);
              L_EXPECT_OP(0, integer);

              auto app = interp_get_app();
              return L_INT(app->gp_.stateflags_.get(L_LOAD_INT(0)));
          }},
         {"save-bit-store",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(0, integer);
              L_EXPECT_OP(1, integer);

              auto app = interp_get_app();
              auto prev = app->gp_.stateflags_.get(L_LOAD_INT(1));

              const bool was_set = app->gp_.stateflags_.get(L_LOAD_INT(1));
              if (not was_set) {
                  app->gp_.stateflags_.set(L_LOAD_INT(1), L_LOAD_INT(0));
                  save::store_global_data(app->gp_);
              }

              return L_INT(prev);
          }},
         {"log",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);

              if (lisp::get_op(0)->type() == lisp::Value::Type::string) {
                  debug(lisp::get_op(0)->string().value());
              } else {
                  lisp::DefaultPrinter p;
                  format(lisp::get_op(0), p);
                  debug(p.data_.c_str());
              }

              return L_NIL;
          }},
         {"fade",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);
              L_EXPECT_OP(0, integer);
              lisp::interp_get_pfrm()->screen().schedule_fade(L_LOAD_INT(0) /
                                                              100.f);
              return L_NIL;
          }},
         {"challenge-complete",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);
              L_EXPECT_OP(0, integer);

              int challenge = lisp::get_op(0)->integer().value_;
              u64 challenge_bitmask = 1 << challenge;

              auto app = interp_get_app();

              const bool was_set =
                  app->gp_.challenge_flags_.get() & challenge_bitmask;

              if (not was_set) {
                  app->gp_.challenge_flags_.set(
                      app->gp_.challenge_flags_.get() | challenge_bitmask);

                  save::store_global_data(interp_get_app()->gp_);
              }


              return L_NIL;
          }},
         {"hibernate",
          [](int argc) {
              lisp::interp_get_pfrm()->system_call("hibernate", nullptr);
              return L_NIL;
          }},
         {"fatal",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);

              lisp::DefaultPrinter p;
              format(lisp::get_op(0), p);
              Platform::fatal(p.data_.c_str());

              return L_NIL;
          }},
         {"now",
          [](int argc) {
              lisp::ListBuilder builder;
              if (auto dt = lisp::interp_get_pfrm()->system_clock().now()) {
                  builder.push_back(lisp::make_integer(dt->date_.year_));
                  builder.push_back(lisp::make_integer(dt->date_.month_));
                  builder.push_back(lisp::make_integer(dt->date_.day_));
                  builder.push_back(lisp::make_integer(dt->hour_));
                  builder.push_back(lisp::make_integer(dt->minute_));
                  builder.push_back(lisp::make_integer(dt->second_));
              }
              return builder.result();
          }},
         {"startup-time",
          [](int argc) {
              lisp::ListBuilder builder;
              auto pfrm = lisp::interp_get_pfrm();
              if (auto dt = pfrm->system_clock().initial_time()) {
                  builder.push_back(lisp::make_integer(dt->date_.year_));
                  builder.push_back(lisp::make_integer(dt->date_.month_));
                  builder.push_back(lisp::make_integer(dt->date_.day_));
                  builder.push_back(lisp::make_integer(dt->hour_));
                  builder.push_back(lisp::make_integer(dt->minute_));
                  builder.push_back(lisp::make_integer(dt->second_));
              }
              return builder.result();
          }},
         {"set-tile",
          [](int argc) {
              L_EXPECT_ARGC(argc, 4);
              L_EXPECT_OP(0, integer);
              L_EXPECT_OP(1, integer);
              L_EXPECT_OP(2, integer);
              L_EXPECT_OP(3, integer);

              lisp::interp_get_pfrm()->set_tile(
                  (Layer)lisp::get_op(3)->integer().value_,
                  lisp::get_op(2)->integer().value_,
                  lisp::get_op(1)->integer().value_,
                  lisp::get_op(0)->integer().value_);

              return L_NIL;
          }},
         {"print",
          [](int argc) {
              L_EXPECT_ARGC(argc, 3);
              L_EXPECT_OP(2, string);
              L_EXPECT_OP(1, integer);
              L_EXPECT_OP(0, integer);

              Text t({L_LOAD_U8(1), L_LOAD_U8(0)});

              t.assign(L_LOAD_STRING(2));

              t.__detach();

              return L_NIL;
          }},
         {"clear",
          [](int argc) {
              lisp::interp_get_pfrm()->screen().clear();
              return L_NIL;
          }},
         {"sound",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);
              L_EXPECT_OP(0, string);
              lisp::interp_get_pfrm()->speaker().play_sound(L_LOAD_STRING(0),
                                                            1);
              return L_NIL;
          }},
         {"display",
          [](int argc) {
              lisp::interp_get_pfrm()->screen().display();
              return L_NIL;
          }},
         {"sbr-annotate",
          [](int argc) {
              lisp::interp_get_pfrm()->system_call("print-memory-diagnostics",
                                                   nullptr);
              return L_NIL;
          }},
         {"readline",
          [](int argc) {
              lisp::interp_get_pfrm()->system_call("feed-watchdog", nullptr);
              auto line = lisp::interp_get_pfrm()->remote_console().readline();
              if (line) {
                  return lisp::make_string(line->c_str());
              }
              return L_NIL;
          }},
         {"printline",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);
              lisp::interp_get_pfrm()->remote_console().printline(
                  L_LOAD_STRING(0));
              return L_NIL;
          }},
         {"pools-annotate",
          [](int argc) {
              GenericPool::print_diagnostics();
              return L_NIL;
          }},
         {"lang",
          [](int argc) {
              return lisp::make_string(systemstring_bound_file());
          }},
         {"synth-notes-store",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              synth_notes_store(*island, L_LOAD_STRING(0));

              return L_NIL;
          }},
         {"synth-notes-load",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              synth_notes_load(*island, L_LOAD_STRING(0));

              return L_NIL;
          }},
         {"speaker-data-store",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              speaker_data_store(*island, L_LOAD_STRING(0));

              return L_NIL;
          }},
         {"speaker-data-load",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              speaker_data_load(*island, L_LOAD_STRING(0));

              return L_NIL;
          }},
         {"room-enable", [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, symbol);
              L_EXPECT_OP(0, integer);

              auto mti = metaclass_index(lisp::get_op(1)->symbol().name());

              set_enabled(mti, L_LOAD_INT(0));

              return L_NIL;
          }}});



extern Sound cannon_sound;
extern Sound missile_sound;



std::pair<App*, Platform*> interp_get_context()
{
    return {interp_get_app(), lisp::interp_get_pfrm()};
}



DynamicMemory<FileLine> get_line_from_file(const char* file_name, int line)
{
    --line; // From the caller's perspective, file lines start from 1.

    auto result = allocate_dynamic<FileLine>("file-line");

    if (!result) {
        return result;
    }

    if (auto contents = PLATFORM.load_file_contents("", file_name)) {

        while (line) {
            while (*contents not_eq '\n' and *contents not_eq '\r') {
                if (*contents == '\0') {
                    return result;
                }
                ++contents;
            }
            ++contents;

            --line;
        }

        while (*contents not_eq '\0' and *contents not_eq '\n') {
            result->push_back(*contents);
            ++contents;
        }
    }

    return result;
}



static HEAP_DATA Buffer<DeferredScene, 8> push_menu_queue;



ScenePtr<Scene> process_script_menu_request()
{
    if (not push_menu_queue.empty()) {
        auto req = std::move(*push_menu_queue.begin());
        push_menu_queue.erase(push_menu_queue.begin());
        return req();
    }
    return null_scene();
}



static const lisp::Binding script_api[] = {
    {"player",
     [](int argc) {
         auto app = interp_get_app();
         return lisp::make_userdata(&app->player_island());
     }},
    {"opponent",
     [](int argc) {
         auto app = interp_get_app();
         if (not app->opponent_island()) {
             if (auto pfrm = lisp::interp_get_pfrm()) {
                 pfrm->fatal("opponent unassigned");
             }
         }
         return lisp::make_userdata(app->opponent_island());
     }},
    {"groups",
     [](int argc) {
         L_EXPECT_ARGC(argc, 0);

         lisp::ListBuilder ret;

         lisp::ListBuilder up;
         lisp::ListBuilder left;
         lisp::ListBuilder right;

         for (auto& room : interp_get_app()->player_island().rooms()) {
             auto pos = room->position();
             switch (room->group()) {
             case Room::Group::count:
             case Room::Group::none:
                 break;

             case Room::Group::one:
                 up.push_back(L_CONS(L_INT(pos.x), L_INT(pos.y)));
                 break;

             case Room::Group::two:
                 right.push_back(L_CONS(L_INT(pos.x), L_INT(pos.y)));
                 break;

             case Room::Group::three:
                 left.push_back(L_CONS(L_INT(pos.x), L_INT(pos.y)));
                 break;
             }
         }

         ret.push_back(L_CONS(L_SYM("Up"), up.result()));
         ret.push_back(L_CONS(L_SYM("Left"), left.result()));
         ret.push_back(L_CONS(L_SYM("Right"), right.result()));

         return ret.result();
     }},
    {"groups-reset",
     [](int argc) {
         L_EXPECT_ARGC(argc, 0);
         for (auto& room : interp_get_app()->player_island().rooms()) {
             room->set_group(Room::Group::none);
         }
         return L_NIL;
     }},
    {"emit",
     [](int argc) {
         L_EXPECT_ARGC(argc, 5);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, integer);
         L_EXPECT_OP(3, integer);
         L_EXPECT_OP(4, user_data);

         auto island = (Island*)lisp::get_op(4)->user_data().obj_;
         u8 x1 = L_LOAD_INT(3);
         u8 y1 = L_LOAD_INT(2);
         u8 x2 = L_LOAD_INT(1);
         u8 y2 = L_LOAD_INT(0);

         auto [app, pfrm] = interp_get_context();

         if (auto room = island->get_room({x1, y1})) {
             if ((*room->metaclass())->category() == Room::Category::weapon) {
                 room->set_target(*app, {x2, y2}, false);
                 if (not str_eq(room->name(), "decimator")) {
                     ((Weapon*)room)->fire(*app);
                 }
                 room->unset_target(*app);
             }
         }

         return L_NIL;
     }},
    {"groups-add",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, symbol);

         u8 x = L_LOAD_INT(1);
         u8 y = L_LOAD_INT(0);

         if (auto room = interp_get_app()->player_island().get_room({x, y})) {
             auto str = lisp::get_op(2)->symbol().name();
             if (str_eq(str, "Up")) {
                 room->set_group(Room::Group::one);
             } else if (str_eq(str, "Left")) {
                 room->set_group(Room::Group::three);
             } else if (str_eq(str, "Right")) {
                 room->set_group(Room::Group::two);
             } else {
                 Platform::fatal(format("invalid group %", str));
             }
         }

         return L_NIL;
     }},
    {"sound",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, string);
         lisp::interp_get_pfrm()->speaker().play_sound(L_LOAD_STRING(0), 1);
         return L_NIL;
     }},
    {"diff",
     [](int argc) {
         auto app = interp_get_app();
         return L_INT((int)app->gp_.difficulty_);
     }},
    {"diff-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);
         auto app = interp_get_app();
         app->gp_.difficulty_ = (GlobalPersistentData::Difficulty)L_LOAD_INT(0);
         return L_NIL;
     }},
    {"mcr-block-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 4);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, integer);
         L_EXPECT_OP(3, integer);

         u8 x = L_LOAD_INT(3);
         u8 y = L_LOAD_INT(2);
         u8 z = L_LOAD_INT(1);
         auto type = (macro::terrain::Type)L_LOAD_INT(0);

         auto& s = macrocosm(*interp_get_app()).sector();

         x %= s.size().x;
         y %= s.size().y;
         z %= s.size().z;

         s.set_block({x, y, z}, type);

         return L_NIL;
     }},
    {"push-menu",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         // L_EXPECT_OP(0, cons);
         L_EXPECT_OP(1, string);

         auto menu_name = L_LOAD_STRING(1);
         auto param_list = lisp::get_op(0);

         using scene_pool::make_deferred_scene;

         if (str_eq(menu_name, "ready")) {
             push_menu_queue.push_back(make_deferred_scene<ReadyScene>());
         } else if (str_eq(menu_name, "item-shop")) {
             push_menu_queue.push_back(make_deferred_scene<ItemShopScene>());
         } else if (str_eq(menu_name, "glossary")) {
             auto sym = param_list->cons().car()->symbol().name();
             push_menu_queue.push_back([sym]() {
                 auto idx = metaclass_index(sym);
                 auto ret = scene_pool::alloc<GlossaryViewerModule>(idx);
                 ret->set_next_scene(make_deferred_scene<ReadyScene>());
                 ret->skip_categories();
                 ret->disable_fade_on_exit_ = true;
                 return ret;
             });
         } else if (str_eq(menu_name, "repl")) {
             push_menu_queue.push_back(make_deferred_scene<LispReplScene>());
         } else if (str_eq(menu_name, "construction")) {
             push_menu_queue.push_back(
                 make_deferred_scene<ConstructionScene>());
         } else if (str_eq(menu_name, "qrcode")) {
             auto tmp = save_str(param_list->cons().car()->string().value());
             push_menu_queue.emplace_back([tmp]() mutable {
                 auto next = scene_pool::alloc<QRViewerScene>(
                     tmp->data_,
                     "",
                     make_deferred_scene<ReadyScene>(),
                     ColorConstant::rich_black);
                 next->set_origin_overworld();
                 return next;
             });
         } else {
             auto tmp = save_str(lisp::get_op(1)->string().value());
             // NOTE: because lisp::Protected is not copyable, there is no way
             // to hide a pure lisp string from the garbage collector when
             // passed through a lambda capture clause.
             push_menu_queue.push_back([tmp]() {
                 return scene_pool::alloc<ScriptedMenuScene>(tmp->data_);
             });
         }
         return L_NIL;
     }},
    {"mcr-block",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, integer);

         u8 x = L_LOAD_INT(2);
         u8 y = L_LOAD_INT(1);
         u8 z = L_LOAD_INT(0);

         auto& sector = macrocosm(*interp_get_app()).sector();
         return L_INT((int)sector.get_block({x, y, z}).type());
     }},
    {"mcr-blocks",
     [](int argc) {
         L_EXPECT_ARGC(argc, 0);
         auto& sector = macrocosm(*interp_get_app()).sector();
         lisp::ListBuilder lat;

         for (u8 z = 0; z < sector.size().z; ++z) {
             for (u8 x = 0; x < sector.size().x; ++x) {
                 for (u8 y = 0; y < sector.size().y; ++y) {
                     auto b = sector.get_block({x, y, z}).type_;
                     if (b not_eq 0) {
                         lisp::ListBuilder sub;
                         sub.push_back(L_INT(b));
                         sub.push_back(L_INT(x));
                         sub.push_back(L_INT(y));
                         sub.push_back(L_INT(z));
                         lat.push_back(sub.result());
                     }
                 }
             }
         }
         return lat.result();
     }},
    {"mcr-sector",
     [](int argc) {
         if (argc == 2) {
             L_EXPECT_OP(0, integer);
             L_EXPECT_OP(1, integer);

             s8 x = L_LOAD_INT(1);
             s8 y = L_LOAD_INT(0);
             if (macrocosm(*interp_get_app()).bind_sector({x, y})) {
                 return L_INT(1);
             }
             return L_NIL;
         } else {
             L_EXPECT_OP(0, string);

             auto str = L_LOAD_STRING(0);

             auto& m = macrocosm(*interp_get_app());
             if (m.data_->origin_sector_->name() == str) {
                 m.bind_sector(m.data_->origin_sector_->coordinate());
                 return L_INT(1);
             }

             for (auto& s : m.data_->other_sectors_) {
                 if (s->name() == str) {
                     m.bind_sector(s->coordinate());
                     return L_INT(1);
                 }
             }
         }

         return L_NIL;
     }},
    {"mcr-sectors",
     [](int argc) {
         L_EXPECT_ARGC(argc, 0);

         auto& m = macrocosm(*interp_get_app());
         lisp::ListBuilder result;

         for (auto& sector : m.data_->other_sectors_) {
             lisp::Protected x(L_INT(sector->coordinate().x));
             lisp::Protected y(L_INT(sector->coordinate().y));
             result.push_back(L_CONS(x, y));
         }

         lisp::Protected x(L_INT(m.data_->origin_sector_->coordinate().x));
         lisp::Protected y(L_INT(m.data_->origin_sector_->coordinate().y));
         result.push_back(L_CONS(x, y));

         return result.result();
     }},
    {"rcnt",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(0, symbol);
         L_EXPECT_OP(1, user_data);

         int count = 0;

         auto island = (Island*)lisp::get_op(1)->user_data().obj_;
         for (auto& room : island->rooms()) {
             if (str_eq(room->name(), lisp::get_op(0)->symbol().name())) {
                 ++count;
             }
         }

         return lisp::make_integer(count);
     }},
    {"room-meta",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, symbol);

         auto mt = load_metaclass(lisp::get_op(0)->symbol().name());

         lisp::ListBuilder b;
         b.push_back(L_CONS(L_SYM("name"),
                            lisp::make_string((*mt)->ui_name()->c_str())));

         auto sz = (*mt)->size();

         b.push_back(L_CONS(L_SYM("size"),
                            L_CONS(L_INT(sz.x), L_INT(sz.y))));

         b.push_back(L_CONS(L_SYM("ico1"),
                            L_INT((*mt)->icon())));

         b.push_back(L_CONS(L_SYM("ico2"),
                            L_INT((*mt)->unsel_icon())));

         b.push_back(L_CONS(L_SYM("pwr"),
                            L_INT((*mt)->consumes_power())));

         return b.result();
     }},
    {"cart-add",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         DataCartLibrary lib;
         lib.store(L_LOAD_INT(0));

         return L_NIL;
     }},
    {"cart-found?",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         DataCartLibrary lib;
         if (lib.load(L_LOAD_INT(0))) {
             return L_INT(1);
         }

         return L_NIL;
     }},
    {"cart-info",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         DataCart cart(L_LOAD_INT(0));
         lisp::ListBuilder list;

         list.push_back(lisp::make_string(cart.name().c_str()));
         list.push_back(lisp::make_string(cart.subheading().c_str()));

         return list.result();
     }},
    {"key-bind",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(1, string);
         L_EXPECT_OP(0, symbol);

         auto s = lisp::get_op(0)->symbol();

         KeyCallbackProcessor::Binding b{KeyCallbackProcessor::MatchSeq{},
                                         [s](App& app) {
                                             // Bad hack: construct dummy symbol.
                                             auto fn = lisp::get_var(s.name());
                                             lisp::safecall(fn, 0);
                                             lisp::pop_op(); // funcall result
                                         }};

         int i = 0;
         auto str = lisp::get_op(1)->string().value();
         while (*str not_eq '\0') {
             if (i == KeyCallbackProcessor::seq_max - 1) {
                 Platform::fatal("too many keys in key-bind expr, max 10");
             }
             if (*str == '-') {
                 ++str;
                 continue;
             }
             b.key_seq_.seq_[i] = [&] {
                 switch (*str) {
                 case 'u':
                     return Key::up;
                 case 'd':
                     return Key::down;
                 case 'l':
                     return Key::left;
                 case 'r':
                     return Key::right;
                 case 'a':
                     return Key::action_1;
                 case 'b':
                     return Key::action_2;
                 default:
                     Platform::fatal("invalid char in key-bind argument.");
                 }
             }();
             ++i;
             ++str;
         }

         interp_get_app()->key_callback_processor().push_binding(b);

         return L_NIL;
     }},
    {"key-reset",
     [](int argc) {
         interp_get_app()->key_callback_processor().clear();
         return L_NIL;
     }},
    {"dialog",
     [](int argc) {
         auto app = interp_get_app();

         for (int i = argc - 1; i > -1; --i) {
             if (not app->dialog_buffer()) {
                 app->dialog_buffer().emplace(
                     allocate_dynamic<DialogString>("dialog-buffer"));
             }

             if (lisp::get_op(i)->type() not_eq lisp::Value::Type::string) {
                 if (lisp::get_op((i)) == L_NIL) {
                     return lisp::get_op((i));
                 } else {
                     return lisp::make_error(
                         lisp::Error::Code::invalid_argument_type, L_NIL);
                 }
             }

             **app->dialog_buffer() += lisp::get_op(i)->string().value();
         }

         return L_NIL;
     }},
    {"fire-new",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer); // y
         L_EXPECT_OP(1, integer); // x
         L_EXPECT_OP(2, user_data);

         auto [app, pfrm] = interp_get_context();

         auto island = (Island*)lisp::get_op(2)->user_data().obj_;

         const u8 x = L_LOAD_INT(1);
         const u8 y = L_LOAD_INT(0);

         island->fire_create(*app, {x, y});

         return L_NIL;
     }},
    {"version",
     [](int argc) {
         L_EXPECT_ARGC(argc, 0);

         lisp::ListBuilder result;
         result.push_back(lisp::make_integer(PROGRAM_MAJOR_VERSION));
         result.push_back(lisp::make_integer(PROGRAM_MINOR_VERSION));
         result.push_back(lisp::make_integer(PROGRAM_SUBMINOR_VERSION));
         result.push_back(lisp::make_integer(PROGRAM_VERSION_REVISION));

         return result.result();
     }},
    {"rooms",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, user_data);

         auto island = (Island*)lisp::get_op(0)->user_data().obj_;

         lisp::ListBuilder result;
         for (auto& room : island->rooms()) {
             lisp::push_op(room->serialize()); // protect from gc.
             result.push_back(lisp::get_op(0));
             lisp::pop_op();
         }

         return result.result();
     }},
    {"chrs",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, user_data);

         auto island = (Island*)lisp::get_op(0)->user_data().obj_;

         lisp::ListBuilder list;

         for (auto& room : island->rooms()) {
             for (auto& chr : room->characters()) {
                 if (chr->owner() == &island->owner()) {
                     using namespace lisp;
                     ListBuilder chr_info;
                     chr_info.push_back(make_integer(chr->grid_position().x));
                     chr_info.push_back(make_integer(chr->grid_position().y));
                     if (chr->health() not_eq 255) {
                         chr_info.push_back(make_cons(
                             make_symbol("hp"), make_integer(chr->health())));
                     }
                     if (chr->is_replicant()) {
                         chr_info.push_back(
                             make_cons(make_symbol("rplc"), make_integer(1)));
                     }
                     if (auto race = chr->get_race()) {
                         chr_info.push_back(make_cons_safe(make_symbol("race"),
                                                           make_integer(race)));
                     }
                     if (auto icon = chr->get_icon()) {
                         chr_info.push_back(make_cons_safe(make_symbol("icon"),
                                                           make_integer(icon)));
                     }
                     chr_info.push_back(
                         make_cons(make_symbol("id"), make_integer(chr->id())));
                     list.push_front(chr_info.result());
                 }
             }
         }
         return list.result();
     }},
    {"chr-slots",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, user_data);

         bool matrix[16][16];

         auto island = (Island*)lisp::get_op(0)->user_data().obj_;

         island->plot_walkable_zones(*interp_get_app(), matrix, nullptr);

         lisp::Value* ret = lisp::get_nil();

         // FIXME: this could in theory return such a large list that we end up
         // with oom errors. But in practice... seems unlikely.

         for (u8 x = 0; x < 16; ++x) {
             for (u8 y = 0; y < 16; ++y) {
                 if (matrix[x][y]) {
                     if (not island->character_at_location({x, y})) {
                         lisp::push_op(ret);
                         {
                             auto cell = lisp::make_cons(L_NIL, L_NIL);
                             lisp::push_op(cell);
                             cell->cons().set_car(lisp::make_integer(x));
                             cell->cons().set_cdr(lisp::make_integer(y));
                             ret = lisp::make_cons(cell, ret);
                             lisp::pop_op(); // cell
                         }
                         lisp::pop_op(); // ret
                     }
                 }
             }
         }
         return ret;
     }},
    {"opponent-mode",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, symbol);

         auto [app, pfrm] = interp_get_context();

         auto conf = lisp::get_op(0);
         if (str_cmp(conf->symbol().name(), "hostile") == 0) {
             app->swap_opponent<EnemyAI>();
         } else if (str_cmp(conf->symbol().name(), "neutral") == 0) {
             app->swap_opponent<FriendlyAI>();
         } else {
             StringBuffer<30> err("bad ai sym: '");
             err += conf->symbol().name();
             pfrm->fatal(err.c_str());
         }

         return L_NIL;
     }},
    {"on-timeout",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(0, symbol);

         auto [app, pfrm] = interp_get_context();

         auto s = lisp::get_op(0)->symbol();

         app->on_timeout(milliseconds(L_LOAD_INT(1)), [s](App&) {
             auto v = lisp::get_var(s.name());
             if (v->type() == lisp::Value::Type::function) {
                 lisp::funcall(v, 0);
                 lisp::pop_op();
             } else {
                 Platform::fatal(format("on-timeout: % is not "
                                        "a function!",
                                        s.name())
                                     .c_str());
             }
         });

         return L_NIL;
     }},
    {"port",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         auto [app, pfrm] = interp_get_context();

         switch (L_LOAD_INT(0)) {
         case 1:
             app->start_console();
             break;
         }

         return L_NIL;
     }},
    {"weather",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         void environment_init(App & app, int type);

         auto [app, pfrm] = interp_get_context();

         environment_init(*app, L_LOAD_INT(0));
         pfrm->screen().set_shader(app->environment().shader(*app));
         pfrm->screen().set_shader_argument(0);

         if (not pfrm->speaker().is_music_playing(app->environment().music())) {
             pfrm->speaker().play_music(app->environment().music(), 0);
         }

         return L_NIL;
     }},
    {"opponent-generate",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         auto [app, pfrm] = interp_get_context();

         // Swap in the procedural-generation enemy AI that we created for
         // SKYLAND Forever, generate a level, and then swap the regular Enemy
         // AI back in.
         auto& o = app->swap_opponent<ProcgenEnemyAI>(
             rng::get(rng::critical_state), 1);

         o.set_levelgen_count(lisp::get_op(0)->integer().value_);
         o.generate_level(*app);

         app->swap_opponent<EnemyAI>();

         return L_NIL;
     }},
    {"opponent-init",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(0, symbol);

         auto [app, pfrm] = interp_get_context();

         auto conf = lisp::get_op(0);
         if (str_cmp(conf->symbol().name(), "hostile") == 0) {
             app->swap_opponent<EnemyAI>();
         } else if (str_cmp(conf->symbol().name(), "neutral") == 0) {
             app->swap_opponent<FriendlyAI>();
         } else {
             StringBuffer<30> err("bad ai sym: '");
             err += conf->symbol().name();
             pfrm->fatal(err.c_str());
         }

         app->create_opponent_island(lisp::get_op(1)->integer().value_);

         return L_NIL;
     }},
    {"terrain",
     [](int argc) {
         if (argc == 2) {
             L_EXPECT_OP(0, integer);
             L_EXPECT_OP(1, user_data);

             auto island = (Island*)lisp::get_op(1)->user_data().obj_;
             island->init_terrain(lisp::get_op(0)->integer().value_);

         } else if (argc == 1) {
             L_EXPECT_OP(0, user_data);

             auto island = (Island*)lisp::get_op(0)->user_data().obj_;
             return lisp::make_integer(island->terrain().size());
         }
         return L_NIL;
     }},
    {"room-new",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(0, cons);
         L_EXPECT_OP(1, user_data);

         auto [app, pfrm] = interp_get_context();

         auto island = (Island*)lisp::get_op(1)->user_data().obj_;
         auto name = lisp::get_list(lisp::get_op(0), 0)->symbol().name();
         u8 x = lisp::get_list(lisp::get_op(0), 1)->integer().value_;
         u8 y = lisp::get_list(lisp::get_op(0), 2)->integer().value_;

         if (auto c = load_metaclass(name)) {
             (*c)->create(*app, island, RoomCoord{x, y});
             island->repaint(*app);
         } else {
             Platform::fatal(name);
         }

         return L_NIL;
     }},
    {"room-del",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, user_data);

         auto [app, pfrm] = interp_get_context();

         auto island = (Island*)lisp::get_op(2)->user_data().obj_;

         auto coord = RoomCoord{
             (u8)lisp::get_op(1)->integer().value_,
             (u8)lisp::get_op(0)->integer().value_,
         };

         island->destroy_room(*app, coord);

         return L_NIL;
     }},
    {"room-mut",
     [](int argc) {
         L_EXPECT_ARGC(argc, 4);
         L_EXPECT_OP(0, symbol);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, integer);
         L_EXPECT_OP(3, user_data);

         auto [app, pfrm] = interp_get_context();

         auto island = (Island*)lisp::get_op(3)->user_data().obj_;

         auto coord = RoomCoord{
             (u8)lisp::get_op(2)->integer().value_,
             (u8)lisp::get_op(1)->integer().value_,
         };

         if (auto room = island->get_room(coord)) {
             auto tp_name = lisp::get_op(0)->symbol().name();
             room->__unsafe__transmute(*app, metaclass_index(tp_name));
         }

         return L_NIL;
     }},
    {"chr-del",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer); // y
         L_EXPECT_OP(1, integer); // x
         L_EXPECT_OP(2, user_data);

         auto island = (Island*)lisp::get_op(2)->user_data().obj_;

         auto coord = RoomCoord{
             (u8)lisp::get_op(1)->integer().value_,
             (u8)lisp::get_op(0)->integer().value_,
         };

         island->remove_character(coord);

         return L_NIL;
     }},
    {"chr-id",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(0, integer); // new-id
         L_EXPECT_OP(1, integer); // old-id

         auto [app, pfrm] = interp_get_context();
         auto old_id = lisp::get_op(1)->integer().value_;
         auto new_id = lisp::get_op(0)->integer().value_;

         if (auto chr = BasicCharacter::find_by_id(*app, old_id).first) {
             chr->__assign_id(new_id);
             BasicCharacter::__rebase_ids(new_id + 1);
         }

         return lisp::get_op(0);
     }},
    {"chr-hp",
     [](int argc) {
         auto [app, pfrm] = interp_get_context();

         if (argc == 2) {
             auto hp = lisp::get_op(0)->integer().value_;
             auto id = lisp::get_op(1)->integer().value_;
             if (auto chr = BasicCharacter::find_by_id(*app, id).first) {
                 chr->__set_health(hp);
             }
         } else if (argc == 1) {
             auto id = lisp::get_op(0)->integer().value_;
             if (auto chr = BasicCharacter::find_by_id(*app, id).first) {
                 return lisp::make_integer(chr->health());
             }
         } else {
             return L_NIL;
         }
         return L_NIL;
     }},
    {"chr-move",
     [](int argc) {
         L_EXPECT_ARGC(argc, 5);
         L_EXPECT_OP(0, integer);   // y2
         L_EXPECT_OP(1, integer);   // x2
         L_EXPECT_OP(2, integer);   // y1
         L_EXPECT_OP(3, integer);   // x1
         L_EXPECT_OP(4, user_data); // island

         auto island = (Island*)lisp::get_op(4)->user_data().obj_;

         u8 startx = lisp::get_op(3)->integer().value_;
         u8 starty = lisp::get_op(2)->integer().value_;

         u8 destx = lisp::get_op(1)->integer().value_;
         u8 desty = lisp::get_op(0)->integer().value_;

         if (auto room = island->get_room({startx, starty})) {
             for (auto& chr : room->characters()) {
                 if (chr->owner() == &room->parent()->owner()) {

                     auto path = find_path(*interp_get_app(),
                                           island,
                                           chr.get(),
                                           {startx, starty},
                                           {destx, desty});

                     if (path and *path) {
                         chr->set_movement_path(*interp_get_app(),
                                                std::move(*path));
                     }

                     break;
                 }
             }
         }
         return L_NIL;
     }},
    {"chr-new",
     [](int argc) {
         L_EXPECT_ARGC(argc, 5);
         L_EXPECT_OP(1, symbol);
         L_EXPECT_OP(2, integer); // y
         L_EXPECT_OP(3, integer); // x
         L_EXPECT_OP(4, user_data);

         auto island = (Island*)lisp::get_op(4)->user_data().obj_;

         auto app = interp_get_app();


         auto coord = RoomCoord{
             (u8)lisp::get_op(3)->integer().value_,
             (u8)lisp::get_op(2)->integer().value_,
         };

         bool is_replicant = false;
         int race = 0;
         int icon = 0;

         // For backwards compatibility with old versions. We used to accept a
         // integer parameter indicating whether the character was a
         // replicant. Now we accept an association list of properties. But we
         // still want to load player's save files.
         if (lisp::get_op(0)->type() == lisp::Value::Type::integer) {
             is_replicant = lisp::get_op0()->integer().value_;
         } else {
             // const bool is_replicant = lisp::get_op(0)->integer().value_;
             if (not lisp::is_list(lisp::get_op(0))) {
                 Platform::fatal("chr-new final arg is not list...");
             }

             lisp::foreach (lisp::get_op(0), [&](lisp::Value* val) {
                 if (val->type() == lisp::Value::Type::cons) {
                     auto car = val->cons().car();
                     if (car->type() == lisp::Value::Type::symbol) {
                         const char* text = car->symbol().name();
                         auto find_param = [&](const char* p) -> int {
                             if (str_eq(p, text)) {
                                 return val->cons().cdr()->integer().value_;
                             }
                             return 0;
                         };

                         if (find_param("rplc")) {
                             is_replicant = true;
                         }
                         if (auto r = find_param("race")) {
                             race = r;
                         }
                         if (auto ic = find_param("icon")) {
                             icon = ic;
                         }
                     }
                 }
             });
         }


         s32 id = -1;

         auto conf = lisp::get_op(1);
         if (str_cmp(conf->symbol().name(), "hostile") == 0) {
             app->swap_opponent<EnemyAI>();
             auto chr = ::skyland::alloc_entity<BasicCharacter>(
                 island, &app->opponent(), coord, is_replicant);

             if (chr) {
                 id = chr->id();

                 chr->set_icon(icon);

                 island->add_character(std::move(chr));
             }

         } else if (str_cmp(conf->symbol().name(), "neutral") == 0) {
             auto chr = ::skyland::alloc_entity<BasicCharacter>(
                 island, &app->player(), coord, is_replicant);

             if (chr) {
                 chr->set_race(race);

                 chr->set_icon(icon);

                 id = chr->id();
                 island->add_character(std::move(chr));
             }
         }

         return lisp::make_integer(id);
     }},
    {"click",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, user_data);

         RoomCoord coord;
         coord.x = lisp::get_op(1)->integer().value_;
         coord.y = lisp::get_op(0)->integer().value_;

         if (auto app = interp_get_app()) {
             if (auto room = ((Island*)lisp::get_op(2)->user_data().obj_)
                                 ->get_room(coord)) {
                 room->select(*app, coord);
             }
         }

         return L_NIL;
     }},
    {"sel-move",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, user_data);

         if (auto app = interp_get_app()) {
             RoomCoord& sel = globals().near_cursor_loc_;

             if (auto ws = app->scene().cast_world_scene()) {
                 if (lisp::get_op(2)->user_data().obj_ ==
                     &app->player_island()) {
                     sel = globals().far_cursor_loc_;
                     ws->near_camera();
                 } else {
                     ws->far_camera();
                 }
             }

             sel.x = lisp::get_op(1)->integer().value_;
             sel.y = lisp::get_op(0)->integer().value_;
         }
         return L_NIL;
     }},
    {"sel-input",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, function);
         L_EXPECT_OP(1, string);

         if (auto app = interp_get_app()) {
             auto bundle = lisp::make_cons(lisp::get_op(1), lisp::get_op(0));
             bundle = lisp::make_cons(bundle, lisp::get_op(2));
             if (bundle->type() == lisp::Value::Type::cons) {
                 app->setup_input(bundle);
             }
         }

         return L_NIL;
     }},
    {"island-configure",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(0, cons);
         L_EXPECT_OP(1, user_data);

         auto [app, pfrm] = interp_get_context();
         auto island = (Island*)lisp::get_op(1)->user_data().obj_;

         configure_island(*app, *island, lisp::get_op(0));

         return L_NIL;
     }},
    {"flag-show",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, user_data);

         auto island = (Island*)lisp::get_op(1)->user_data().obj_;
         island->show_flag(true);

         auto gfx = L_LOAD_INT(0);
         auto [app, pfrm] = interp_get_context();
         if (gfx > 0 and island == &app->player_island()) {
             Platform::fatal("Alternate flag graphics may not be assigned to "
                             "the player's island, as it already supports "
                             "user-supplied gfx.");
         }
         island->set_custom_flag_graphics(gfx);

         if (island->interior_visible()) {
             show_island_interior(*app, island);
         } else {
             show_island_exterior(*app, island);
         }

         return L_NIL;
     }},
    {"exit",
     [](int argc) {
         if (argc == 1) {
             L_EXPECT_OP(0, integer);
             interp_get_app()->exit_condition() =
                 (App::ExitCondition)L_LOAD_INT(0);
         } else {
             interp_get_app()->exit_condition() = App::ExitCondition::misc;
         }
         return L_NIL;
     }},
    {"coins",
     [](int argc) {
         auto app = interp_get_app();
         if (app->macrocosm()) {
             // return lisp::make_integer(macrocosm(*app).data_->p().coins_.get());
             return L_NIL;
         } else {
             return lisp::make_integer(app->coins());
         }
     }},
    {"coins-add",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         auto app = interp_get_app();

         if (app->macrocosm()) {
             // auto current = macrocosm(*app).data_->p().coins_.get();
             // current += L_LOAD_INT(0);
             // macrocosm(*app).data_->p().coins_.set(
             //     std::min(std::numeric_limits<macro::Coins>::max(), current));
         } else {
             app->set_coins(std::max(0, (int)(L_LOAD_INT(0) + app->coins())));
         }

         return L_NIL;
     }},
    {"coins-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         auto app = interp_get_app();
         if (app->macrocosm()) {
             // auto val = L_LOAD_INT(0);
             // macrocosm(*app).data_->p().coins_.set(
             //     std::min(std::numeric_limits<macro::Coins>::max(), val));
         } else {
             app->set_coins(L_LOAD_INT(0));
         }


         return L_NIL;
     }},
    {"coins-victory",
     [](int argc) { return L_INT(interp_get_app()->victory_coins()); }},
    {"eval-file",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, string);

         auto app = interp_get_app();
         if (app == nullptr) {
             while (true)
                 ;
             return L_NIL;
         }

         auto str = lisp::get_op(0)->string().value();

         return app->invoke_script(str);
     }},
    {"choice",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);
         return lisp::make_integer(rng::choice(
             lisp::get_op(0)->integer().value_, rng::critical_state));
     }},
    {"autopilot",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, cons);

         interp_get_app()->swap_player<AutopilotPlayer>(lisp::get_op(0));

         return L_NIL;
     }},
    {"get-line-of-file",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(1, string);
         L_EXPECT_OP(0, integer);

         auto line = get_line_from_file(lisp::get_op(1)->string().value(),
                                        lisp::get_op(0)->integer().value_);

         if (line) {
             info(line->c_str());
             return lisp::make_string(line->c_str());
         }

         return L_NIL;
     }},
    {"configure-rooms",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, cons);

         lisp::foreach (lisp::get_op(0), [](lisp::Value* val) {
             if (val->type() not_eq lisp::Value::Type::cons) {
                 return;
             }

             auto name_sym = val->cons().car();
             if (name_sym->type() not_eq lisp::Value::Type::symbol) {
                 return;
             }

             val = val->cons().cdr();

             if (auto c = load_metaclass(name_sym->symbol().name())) {
                 auto health = val->cons().car()->integer().value_;
                 val = val->cons().cdr();
                 auto cost = val->cons().car()->integer().value_;
                 val = val->cons().cdr();
                 auto power = val->cons().car()->integer().value_;
                 (*c)->configure(health, cost, power);
             } else {
                 Platform::fatal("invalid room type symbol");
             }
         });

         return L_NIL;
     }},
    {"syscall",
     [](int argc) {
         if (argc < 1) {
             Platform::fatal("invalid argc passed to syscall");
         }
         L_EXPECT_OP(argc - 1, string);


         auto name = lisp::get_op(argc - 1)->string().value();


         Buffer<lisp::Value*, 10> args;
         for (int i = argc - 2; i > -1; --i) {
             args.push_back(lisp::get_op(i));
         }

         for (auto& arg : args) {
             lisp::push_op(arg);
         }

         auto result = L_NIL;

         auto found_syscall = syscall_table.find(name);
         if (found_syscall not_eq syscall_table.end()) {
             result = found_syscall->second(args.size());
         } else {
             Platform::fatal("syscall function lookup failed!");
         }

         for (auto& arg : args) {
             (void)arg;
             lisp::pop_op();
         }

         return result;
     }},
    {"cargo",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer); // y
         L_EXPECT_OP(1, integer); // x
         L_EXPECT_OP(2, user_data);

         auto island = (Island*)lisp::get_op(2)->user_data().obj_;
         const u8 x = lisp::get_op(1)->integer().value_;
         const u8 y = lisp::get_op(0)->integer().value_;

         if (auto room = island->get_room({x, y})) {
             if (auto cb = room->cast<CargoBay>()) {
                 if (*cb->cargo() not_eq '\0') {
                     return lisp::make_string(cb->cargo());
                 }
             } else {
                 Platform::fatal("Requested cargo from "
                                 "non cargo-bay room.");
             }
         }
         return L_NIL;
     }},
    {"cargo-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 4);
         L_EXPECT_OP(0, string);  // cargo
         L_EXPECT_OP(1, integer); // y
         L_EXPECT_OP(2, integer); // x
         L_EXPECT_OP(3, user_data);

         auto island = (Island*)lisp::get_op(3)->user_data().obj_;
         const u8 x = lisp::get_op(2)->integer().value_;
         const u8 y = lisp::get_op(1)->integer().value_;

         if (auto room = island->get_room({x, y})) {
             if (auto cb = room->cast<CargoBay>()) {
                 cb->set_cargo(lisp::get_op(0)->string().value(), 1);
             }
         }

         return L_NIL;
     }},
    {"qr-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 4);
         L_EXPECT_OP(0, string);  // cargo
         L_EXPECT_OP(1, integer); // y
         L_EXPECT_OP(2, integer); // x
         L_EXPECT_OP(3, user_data);

         auto island = (Island*)lisp::get_op(3)->user_data().obj_;
         const u8 x = lisp::get_op(2)->integer().value_;
         const u8 y = lisp::get_op(1)->integer().value_;

         if (auto room = island->get_room({x, y})) {
             if (auto cb = room->cast<QrBlock>()) {
                 cb->set_message(lisp::get_op(0)->string().value());
             }
         }

         return L_NIL;
     }},
    {"achieve",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         const auto achievement =
             (achievements::Achievement)(lisp::get_op(0)->integer().value_);

         if ((int)achievement >= (int)achievements::Achievement::count) {
             return L_NIL;
         }

         achievements::raise(*interp_get_app(), achievement);

         return L_NIL;
     }},
    {"wg-nodes",
     [](int argc) {
         lisp::ListBuilder builder;
         for (auto& node : interp_get_app()->world_graph().nodes_) {
             if (node.type_ not_eq WorldGraph::Node::Type::null) {
                 builder.push_back(L_CONS(
                     L_INT((int)node.type_),
                     L_CONS(L_INT(node.coord_.x), L_INT(node.coord_.y))));
             }
         }
         return builder.result();
     }},
    {"wg-node-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(0, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(2, integer);

         const s8 x = L_LOAD_U8(2);
         const s8 y = L_LOAD_U8(1);

         for (auto& node : interp_get_app()->world_graph().nodes_) {
             if (node.coord_ == Vec2<s8>{x, y}) {
                 node.type_ = (WorldGraph::Node::Type)L_LOAD_U8(0);
                 break;
             }
         }

         return L_NIL;
     }},
    {"wg-pos",
     [](int argc) {
         auto app = interp_get_app();

         const auto& node =
             app->world_graph().nodes_[app->current_world_location()];

         return L_CONS(L_INT(app->zone() - 1),
                       L_CONS(L_INT(node.coord_.x), L_INT(node.coord_.y)));
     }},
    {"gui-add-node",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(0, string);
         L_EXPECT_OP(1, string);
         auto app = interp_get_app();
         app->scene().gui_add_node(nullptr,
                                   L_LOAD_STRING(1),
                                   L_LOAD_STRING(0));
         return L_NIL;
     }},
    {"gui-delete-node",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, string);
         auto app = interp_get_app();
         app->scene().gui_delete_node(L_LOAD_STRING(0));
         return L_NIL;
     }},
    {"gui-set-attr",
     [](int argc) {
         L_EXPECT_ARGC(argc, 3);
         L_EXPECT_OP(1, string);
         L_EXPECT_OP(2, string);
         auto app = interp_get_app();
         app->scene().gui_set_attr(L_LOAD_STRING(2),
                                   L_LOAD_STRING(1),
                                   lisp::get_op0());
         return L_NIL;
     }},
    {"construction-sites",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(1, user_data);
         L_EXPECT_OP(0, cons);

         const int sx = lisp::get_op(0)->cons().car()->integer().value_;
         const int sy = lisp::get_op(0)->cons().cdr()->integer().value_;

         auto island = (Island*)lisp::get_op(1)->user_data().obj_;

         auto matrix = allocate_dynamic<bool[16][16]>("construction-zones");

         island->plot_construction_zones(*matrix);

         lisp::ListBuilder builder;

         for (u32 x = 0; x < island->terrain().size(); ++x) {
             for (int y = construction_zone_min_y + 1; y < 15; ++y) {
                 if ((*matrix)[x][y] and y - (sy - 1) > 0) {
                     bool has_space = true;
                     for (int xx = 0; xx < sx and x + xx < 16; ++xx) {
                         for (int yy = 0; yy < sy and y - yy > 0; ++yy) {
                             if (island->rooms_plot().get(x + xx, y - yy)) {
                                 has_space = false;
                             }
                             if (x + xx >= island->terrain().size()) {
                                 has_space = false;
                             }
                         }
                     }
                     if (has_space) {
                         builder.push_back(
                             L_CONS(L_INT(x), L_INT(y - (sy - 1))));
                     }
                 }
             }
         }

         return builder.result();
     }},
};



void App::init_scripts(Function<4 * sizeof(void*), void(const char*)> msg)
{
    msg("lisp init...");

    lisp::init();

    msg("export api...");

    lisp::bind_functions(script_api,
                         sizeof(script_api) / sizeof(script_api[0]));

    __app = this;

    // NOTE: we need to disable custom scripts during startup, otherwise,
    // someone could irreversibly mess up a game.
    const bool was_developer_mode = is_developer_mode();
    set_developer_mode(false);

    msg("import lisp stdlib...");

    auto str = PLATFORM.load_file_contents("scripts", "init.lisp");
    if (str) {
        lisp::BasicCharSequence seq(str);
        lisp::dostring(seq, [](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            PLATFORM.fatal(p.data_.c_str());
        });
    }

    set_developer_mode(was_developer_mode);
}



} // namespace skyland
