////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "alloc_entity.hpp"
#include "allocator.hpp"
#include "configure_island.hpp"
#include "dataCart.hpp"
#include "eternal/eternal.hpp"
#include "ext_workram_data.hpp"
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
#include "scene/loadLevelScene.hpp"
#include "scene/modules/fileBrowserModule.hpp"
#include "scene/modules/hexViewerModule.hpp"
#include "scene/qrViewerScene.hpp"
#include "scene/readyScene.hpp"
#include "scene/scriptHookScene.hpp"
#include "scene/scriptedMenuScene.hpp"
#include "scene/startAdventureScene.hpp"
#include "scene/textEntryScene.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "serial.hpp"
#include "sharedVariable.hpp"
#include "skyland.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/misc/lightningStrike.hpp"
#include "skyland/rooms/weapon.hpp"
#include "skyland/scene/desktopOS.hpp"
#include "skyland/scene/itemShopScene.hpp"
#include "skyland/scene/lispReplScene.hpp"
#include "skyland/scene/modules/glossaryViewerModule.hpp"
#include "skyland/scene/worldMapScene.hpp"
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



extern Sound cannon_sound;
extern Sound missile_sound;



bool is_x_behind_storm_frontier(int x, int storm_offset);
void shift_rooms_right(Island& island);



std::pair<App*, Platform*> interp_get_context()
{
    return {&APP, lisp::interp_get_pfrm()};
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



void restore_overworld_textures();



static EXT_WORKRAM_DATA Buffer<DeferredScene, 8> push_menu_queue;



ScenePtr process_script_menu_request()
{
    if (not push_menu_queue.empty()) {
        auto req = std::move(*push_menu_queue.begin());
        push_menu_queue.erase(push_menu_queue.begin());
        return req();
    }
    return null_scene();
}



using Binding = lisp::NativeInterface::LookupResult;


#ifdef __GBA__
#define BINDING_TABLE                                                          \
    MAPBOX_ETERNAL_CONSTEXPR const auto binding_table =                        \
        mapbox::eternal::hash_map<mapbox::eternal::string, Binding>

#define SYMTAB                                                                 \
    MAPBOX_ETERNAL_CONSTEXPR const auto symtab =                               \
        mapbox::eternal::hash_map<mapbox::eternal::string, int>
#else
#define BINDING_TABLE                                                          \
    const auto binding_table = std::unordered_map<std::string, Binding>
#define SYMTAB                                                                 \
    const auto symtab = std::unordered_map<mapbox::eternal::string, int>
#endif


SYMTAB({{"on-fadein", 0},
        {"on-converge", 0},
        {"on-room-destroyed", 0},
        {"on-crew-died", 0},
        {"on-shop-item-sel", 0}});



lisp::Value* wrap_island(Island* isle)
{
    auto tag = isle->script_userdata_tag();
    lisp::Protected ud(lisp::make_userdata(isle, tag));
    return lisp::wrap(ud, L_SYM("isle"));
}



int file_line_count(const char* fname)
{
    int lcnt = 0;
    if (auto contents = PLATFORM.load_file_contents("", fname)) {
        while (*contents not_eq '\0') {
            if (*contents == '\n') {
                ++lcnt;
            }
            ++contents;
        }
    }
    return lcnt;
}



Island* unwrap_isle(lisp::Value* v)
{
    if (not str_eq(dcompr(v->wrapped().type_sym_)->symbol().name(), "isle")) {
        PLATFORM.fatal("invalid wrapped value!?");
    }
    return (Island*)dcompr(v->wrapped().data_)->user_data().obj_;
}


BINDING_TABLE({
    {"player",
     {SIG0(wrapped),
      [](int argc) { return wrap_island(&APP.player_island()); }}},
    {"opponent",
     {SIG0(wrapped),
      [](int argc) {
          if (not APP.opponent_island()) {
              if (auto pfrm = lisp::interp_get_pfrm()) {
                  pfrm->fatal("opponent unassigned");
              }
          }
          return wrap_island(APP.opponent_island());
      }}},
    {"-decorate-isle",
     {SIG1(string, wrapped),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);
          auto island = unwrap_isle(lisp::get_op(0));
          auto name = is_player_island(island) ? "player" : "opponent";
          return lisp::make_string(format("#(isle:%)", name).c_str());
      }}},
    {"-equal-isle",
     {EMPTY_SIG(2),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);
          L_EXPECT_OP(1, wrapped);
          auto island = unwrap_isle(lisp::get_op(0));
          auto island2 = unwrap_isle(lisp::get_op(1));
          return lisp::make_boolean(island == island2);
      }}},
    {"lang-set",
     {SIG1(nil, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          systemstring_bind_file(L_LOAD_STRING(0));
          return L_NIL;
      }}},
    {"lang",
     {SIG0(string),
      [](int argc) { return lisp::make_string(systemstring_bound_file()); }}},
    {"log",
     {SIG1(nil, nil),
      [](int argc) {
          if (lisp::get_op(0)->type() == lisp::Value::Type::string) {
              debug(lisp::get_op(0)->string().value());
          } else {
              lisp::DefaultPrinter p;
              format(lisp::get_op(0), p);
              debug(p.data_.c_str());
          }

          return L_NIL;
      }}},
    {"score",
     {SIG0(integer), [](int argc) { return L_INT(APP.score().get()); }}},
    {"score-add",
     {SIG1(integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          APP.score().set(APP.score().get() + L_LOAD_INT(0));
          return L_INT(APP.score().get());
      }}},
    {"groups",
     {SIG0(cons),
      [](int argc) {
          lisp::ListBuilder ret;

          lisp::ListBuilder up;
          lisp::ListBuilder left;
          lisp::ListBuilder right;
          lisp::ListBuilder poweroff;

          for (auto& room : APP.player_island().rooms()) {
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

              if (room->is_powered_down()) {
                  poweroff.push_back(L_CONS(L_INT(pos.x), L_INT(pos.y)));
              }
          }

          ret.push_back(L_CONS(L_SYM("Up"), up.result()));
          ret.push_back(L_CONS(L_SYM("Left"), left.result()));
          ret.push_back(L_CONS(L_SYM("Right"), right.result()));
          ret.push_back(L_CONS(L_SYM("poweroff"), poweroff.result()));

          return ret.result();
      }}},
    {"groups-reset",
     {SIG0(nil),
      [](int argc) {
          for (auto& room : APP.player_island().rooms()) {
              room->set_group(Room::Group::none);
          }
          return L_NIL;
      }}},
    {"repaint",
     {SIG0(nil),
      [](int argc) {
          APP.player_island().repaint();
          return L_NIL;
      }}},
    {"emit",
     {EMPTY_SIG(5),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, integer);
          L_EXPECT_OP(3, integer);
          L_EXPECT_OP(4, wrapped);

          auto island = unwrap_isle(lisp::get_op(4));
          u8 x1 = L_LOAD_INT(3);
          u8 y1 = L_LOAD_INT(2);
          u8 x2 = L_LOAD_INT(1);
          u8 y2 = L_LOAD_INT(0);

          if (auto room = island->get_room({x1, y1})) {
              if ((*room->metaclass())->category() == Room::Category::weapon) {
                  room->set_target({x2, y2}, false);
                  if (auto w = room->cast_weapon()) {
                      w->fire();
                  }
                  room->unset_target();
              }
          }

          return L_NIL;
      }}},
    {"groups-add",
     {SIG3(nil, symbol, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, symbol);

          u8 x = L_LOAD_INT(1);
          u8 y = L_LOAD_INT(0);

          if (auto room = APP.player_island().get_room({x, y})) {
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
      }}},
    {"sound",
     {SIG1(nil, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          lisp::interp_get_pfrm()->speaker().play_sound(L_LOAD_STRING(0), 6);
          return L_NIL;
      }}},
    {"difficulty",
     {SIG0(integer), [](int argc) { return L_INT((int)APP.gp_.difficulty_); }}},
    {"difficulty-set",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          APP.gp_.difficulty_ = (GlobalPersistentData::Difficulty)L_LOAD_INT(0);
          return L_NIL;
      }}},
    {"mcr-block-set",
     {EMPTY_SIG(4),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, integer);
          L_EXPECT_OP(3, integer);

          u8 x = L_LOAD_INT(3);
          u8 y = L_LOAD_INT(2);
          u8 z = L_LOAD_INT(1);
          auto type = (macro::terrain::Type)L_LOAD_INT(0);

          auto& s = macrocosm().sector();

          x %= s.size().x;
          y %= s.size().y;
          z %= s.size().z;

          s.set_block({x, y, z}, type);

          return L_NIL;
      }}},
    {"nimbus",
     {SIG0(nil),
      [](int argc) {
          push_menu_queue.emplace_back([] {
              return boot_desktop_os([] {
                  restore_overworld_textures();
                  if (APP.opponent_island()) {
                      show_island(APP.opponent_island());
                  }
                  PLATFORM_EXTENSION(force_vsync);
                  PLATFORM.screen().fade(
                      1, ColorConstant::rich_black, {}, true, true);
                  PLATFORM.load_sprite_texture("spritesheet");
                  PLATFORM.load_overlay_texture("overlay");
                  PLATFORM.set_overlay_origin(0, 0);
                  PLATFORM.speaker().stream_music(
                      APP.environment().music()->c_str(), 0);
                  return make_scene<LispReplScene>();
              });
          });
          return L_NIL;
      }}},
    {"help",
     {SIG0(nil),
      [](int argc) {
          if (argc == 0) {
              push_menu_queue.emplace_back([] {
                  UserContext ctx;
                  ctx.browser_exit_scene_ = make_deferred_scene<ReadyScene>();
                  return make_scene<FileBrowserModule>(
                      std::move(ctx), "/help/", true);
              });
          }
          return L_NIL;
      }}},
    {"push-menu",
     {SIG2(nil, string, nil),
      [](int argc) {
          L_EXPECT_OP(1, string);

          auto menu_name = L_LOAD_STRING(1);
          auto param_list = lisp::get_op(0);

          if (str_eq(menu_name, "ready")) {
              push_menu_queue.push_back(make_deferred_scene<ReadyScene>());
          } else if (str_eq(menu_name, "item-shop")) {
              push_menu_queue.push_back(make_deferred_scene<ItemShopScene>());
          } else if (str_eq(menu_name, "file-browser")) {
              auto path = get_list(param_list, 0)->string().value();
              bool allow_backtrack = get_list(param_list, 1)->integer().value_;
              push_menu_queue.push_back([path, allow_backtrack] {
                  UserContext ctx;
                  ctx.allow_backtrack_ = allow_backtrack;
                  ctx.browser_exit_scene_ = make_deferred_scene<ReadyScene>();
                  auto next =
                      make_scene<FileBrowserModule>(std::move(ctx), path, true);
                  next->on_select_ = [](const char* path) {
                      auto fn = lisp::get_var("on-menu-resp");
                      lisp::push_op(lisp::make_string(path));
                      lisp::safecall(fn, 1);
                      lisp::pop_op(); // discard result
                  };
                  return next;
              });
          } else if (str_eq(menu_name, "text-entry")) {
              auto rqd = get_list(param_list, 0)->integer().value_;
              auto lim = get_list(param_list, 1)->integer().value_;
              push_menu_queue.push_back([rqd, lim]() {
                  return make_scene<TextEntryScene>(
                      "Entry:",
                      [](const char* text) {
                          auto fn = lisp::get_var("on-menu-resp");
                          lisp::push_op(lisp::make_string(text));
                          lisp::safecall(fn, 1);
                          lisp::pop_op(); // ignore result
                          return make_scene<ReadyScene>();
                      },
                      rqd,
                      lim);
              });
          } else if (str_eq(menu_name, "glossary")) {
              auto sym = param_list->cons().car()->symbol().name();
              push_menu_queue.push_back([sym]() {
                  auto idx = metaclass_index(sym);
                  auto ret = make_scene<GlossaryViewerModule>(idx);
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
                  auto next = make_scene<QRViewerScene>(
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
                  return make_scene<ScriptedMenuScene>(tmp->data_);
              });
          }
          return L_NIL;
      }}},
    {"mcr-block",
     {SIG3(integer, integer, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, integer);

          u8 x = L_LOAD_INT(2);
          u8 y = L_LOAD_INT(1);
          u8 z = L_LOAD_INT(0);

          auto& sector = macrocosm().sector();
          return L_INT((int)sector.get_block({x, y, z}).type());
      }}},
    {"mcr-blocks",
     {SIG0(cons),
      [](int argc) {
          auto& sector = macrocosm().sector();
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
      }}},
    {"mcr-sector",
     {EMPTY_SIG(1),
      [](int argc) {
          if (argc == 2) {
              L_EXPECT_OP(0, integer);
              L_EXPECT_OP(1, integer);

              s8 x = L_LOAD_INT(1);
              s8 y = L_LOAD_INT(0);
              if (macrocosm().bind_sector({x, y})) {
                  return L_INT(1);
              }
              return L_NIL;
          } else {
              L_EXPECT_OP(0, string);

              auto str = L_LOAD_STRING(0);

              auto& m = macrocosm();
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
      }}},
    {"mcr-sectors",
     {EMPTY_SIG(0),
      [](int argc) {
          auto& m = macrocosm();
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
      }}},
    {"room-damage",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));

          auto coord = RoomCoord{
              (u8)lisp::get_op(1)->integer().value_,
              (u8)lisp::get_op(0)->integer().value_,
          };

          if (auto room = island->get_room(coord)) {
              auto mt = load_metaclass(room->metaclass_index());
              return L_INT((*mt)->full_health() - room->health());
          } else {
              return L_NIL;
          }
      }}},
    {"room-hp",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));

          auto coord = RoomCoord{
              (u8)lisp::get_op(1)->integer().value_,
              (u8)lisp::get_op(0)->integer().value_,
          };

          if (auto room = island->get_room(coord)) {
              return L_INT(room->health());
          } else {
              return L_NIL;
          }
      }}},
    {"room-hp-set",
     {SIG4(nil, wrapped, integer, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, integer);
          L_EXPECT_OP(3, wrapped);

          auto island = unwrap_isle(lisp::get_op(3));

          auto coord = RoomCoord{
              (u8)lisp::get_op(2)->integer().value_,
              (u8)lisp::get_op(1)->integer().value_,
          };

          if (auto room = island->get_room(coord)) {
              auto hp = room->health();
              auto newhp = lisp::get_op(0)->integer().value_;

              auto diff = newhp - hp;
              if (diff < 0) {
                  room->apply_damage(abs(diff));
              } else {
                  room->heal(diff);
              }
              island->schedule_repaint();
          }

          return L_NIL;
      }}},
    {"room-count",
     {SIG2(integer, wrapped, symbol),
      [](int argc) {
          L_EXPECT_OP(0, symbol);
          L_EXPECT_OP(1, wrapped);

          int count = 0;

          auto island = unwrap_isle(lisp::get_op(1));
          for (auto& room : island->rooms()) {
              if (str_eq(room->name(), lisp::get_op(0)->symbol().name())) {
                  ++count;
              }
          }

          return lisp::make_integer(count);
      }}},
    {"room-meta",
     {SIG1(cons, symbol),
      [](int argc) {
          L_EXPECT_OP(0, symbol);

          auto mt = load_metaclass(lisp::get_op(0)->symbol().name());

          lisp::ListBuilder b;

          auto append = [&](const char* sym, lisp::Value* v) {
              lisp::Protected p(v);
              b.push_back(L_CONS(L_SYM(sym), v));
          };

          append("name", lisp::make_string((*mt)->ui_name()->c_str()));

          auto sz = (*mt)->constructed_size();

          append("size", L_CONS(L_INT(sz.x), L_INT(sz.y)));
          append("ico1", L_INT((*mt)->icon()));
          append("ico2", L_INT((*mt)->unsel_icon()));
          append("pwr", L_INT((*mt)->consumes_power()));
          append("cost", L_INT((*mt)->cost()));
          append("max-hp", L_INT((*mt)->full_health()));
          append("category", L_SYM([mt] {
                     switch ((*mt)->category()) {
                     case Room::Category::wall:
                         return "wall";
                     case Room::Category::weapon:
                         return "weapon";
                     case Room::Category::factory:
                         return "factory";
                     case Room::Category::power:
                         return "power";
                     case Room::Category::misc:
                         return "misc";
                     case Room::Category::decoration:
                         return "decoration";
                     default:
                         return "invalid?!";
                     }
                 }()));

          return b.result();
      }}},
    {"cart-add",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          DataCartLibrary lib;
          lib.store(L_LOAD_INT(0));

          return L_NIL;
      }}},
    {"cart-found?",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          DataCartLibrary lib;
          if (lib.load(L_LOAD_INT(0))) {
              return L_INT(1);
          }

          return L_NIL;
      }}},
    {"cart-read",
     {SIG2(string, integer, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          L_EXPECT_OP(1, integer);

          DataCart cart(L_LOAD_INT(1));

          if (auto str = (cart.get_content_string(L_LOAD_STRING(0)))) {
              return lisp::make_string((*str)->c_str());
          } else {
              return L_NIL;
          }
      }}},
    {"cart-info",
     {SIG1(cons, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          DataCart cart(L_LOAD_INT(0));
          lisp::ListBuilder list;

          list.push_back(lisp::make_string(cart.name().c_str()));
          list.push_back(lisp::make_string(cart.subheading().c_str()));

          return list.result();
      }}},
    {"show-fps",
     {SIG1(nil, nil),
      [](int argc) {
          state_bit_store(StateBit::show_fps,
                          lisp::is_boolean_true(lisp::get_op0()));
          return L_NIL;
      }}},
    {"key-bind",
     {SIG2(nil, string, symbol),
      [](int argc) {
          L_EXPECT_OP(1, string);
          L_EXPECT_OP(0, symbol);

          auto s = lisp::get_op(0)->symbol();

          KeyCallbackProcessor::Binding b{KeyCallbackProcessor::MatchSeq{},
                                          [s]() {
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

          APP.key_callback_processor().push_binding(b);

          return L_NIL;
      }}},
    {"key-reset",
     {SIG0(nil),
      [](int argc) {
          APP.key_callback_processor().clear();
          return L_NIL;
      }}},
    {"has-dialog?",
     {SIG0(nil),
      [](int argc) { return lisp::make_boolean((bool)APP.dialog_buffer()); }}},
    {"dialog-reset",
     {SIG0(nil),
      [](int argc) {
          APP.dialog_buffer().reset();
          return L_NIL;
      }}},
    {"dialog",
     {SIG1(nil, string),
      [](int argc) {
          for (int i = argc - 1; i > -1; --i) {
              if (not APP.dialog_buffer()) {
                  APP.dialog_buffer().emplace(
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

              **APP.dialog_buffer() += lisp::get_op(i)->string().value();
          }

          return L_NIL;
      }}},
    {"fire-new",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer); // y
          L_EXPECT_OP(1, integer); // x
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));

          const u8 x = L_LOAD_INT(1);
          const u8 y = L_LOAD_INT(0);

          island->fire_create({x, y});

          return L_NIL;
      }}},
    {"version",
     {SIG0(cons),
      [](int argc) {
          lisp::ListBuilder result;
          result.push_back(lisp::make_integer(PROGRAM_MAJOR_VERSION));
          result.push_back(lisp::make_integer(PROGRAM_MINOR_VERSION));
          result.push_back(lisp::make_integer(PROGRAM_SUBMINOR_VERSION));
          result.push_back(lisp::make_integer(PROGRAM_VERSION_REVISION));

          return result.result();
      }}},
    {"poweroff",
     {SIG4(nil, wrapped, integer, integer, integer),
      [](int argc) {
          L_EXPECT_OP(3, wrapped);
          L_EXPECT_OP(2, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(0, integer);

          auto island = unwrap_isle(lisp::get_op(3));
          u8 x = L_LOAD_INT(2);
          u8 y = L_LOAD_INT(1);
          auto on = L_LOAD_INT(0);

          if (auto room = island->get_room({x, y})) {
              if (room->allows_powerdown()) {
                  room->set_powerdown(on);
              }
          }

          return L_NIL;
      }}},
    {"rooms",
     {SIG1(cons, wrapped),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);

          auto island = unwrap_isle(lisp::get_op(0));

          lisp::ListBuilder result;
          for (auto& room : island->rooms()) {
              lisp::push_op(room->serialize()); // protect from gc.
              result.push_back(lisp::get_op(0));
              lisp::pop_op();
          }

          return result.result();
      }}},
    {"rooms-damaged",
     {SIG1(cons, wrapped),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);

          auto island = unwrap_isle(lisp::get_op(0));

          lisp::ListBuilder result;
          for (auto& room : island->rooms()) {
              auto mt = load_metaclass(room->metaclass_index());
              if ((*mt)->full_health() not_eq room->health()) {
                  result.push_back(L_CONS(L_INT(room->position().x),
                                          L_INT(room->position().y)));
              }
          }

          return result.result();
      }}},
    {"chrs",
     {SIG1(cons, wrapped),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);

          auto island = unwrap_isle(lisp::get_op(0));

          lisp::ListBuilder list;

          for (auto& room : island->rooms()) {
              for (auto& chr : room->characters()) {
                  if (chr->owner() == &island->owner()) {
                      list.push_front(chr->serialize());
                  }
              }
          }
          return list.result();
      }}},
    {"chr-find",
     {SIG3(nil, wrapped, symbol, nil),
      [](int argc) {
          lisp::ListBuilder result;

          auto sym = lisp::get_op1()->symbol().name();
          int match = L_LOAD_INT(0);

          auto island = unwrap_isle(lisp::get_op(2));
          for (auto& room : island->rooms()) {
              for (auto& chr : room->characters()) {
                  auto trymatch = [&](const char* key, int val) {
                      if (str_eq(sym, key)) {
                          return val == match;
                      } else {
                          return false;
                      }
                  };
                  if (trymatch("race", (int)chr->get_race()) or
                      trymatch("icon", chr->get_icon()) or
                      trymatch("id", chr->id()) or
                      trymatch("rplc", chr->is_replicant())) {
                      result.push_back(chr->serialize());
                  }
              }
          }
          return result.result();
      }}},
    {"chr-slots",
     {SIG0(cons),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);

          bool matrix[16][16];

          auto island = unwrap_isle(lisp::get_op(0));

          island->plot_walkable_zones(matrix, nullptr);

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
      }}},
    {"opponent-mode",
     {SIG1(nil, symbol),
      [](int argc) {
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
      }}},
    {"on-timeout",
     {SIG2(nil, integer, symbol),
      [](int argc) {
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(0, symbol);

          auto [app, pfrm] = interp_get_context();

          auto s = lisp::get_op(0)->symbol();

          app->on_timeout(milliseconds(L_LOAD_INT(1)), [s]() {
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
      }}},
    {"port",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          auto [app, pfrm] = interp_get_context();

          switch (L_LOAD_INT(0)) {
          case 1:
              app->start_console();
              break;
          }

          return L_NIL;
      }}},
    {"effect",
     {SIG3(nil, string, integer, integer),
      [](int argc) {
          L_EXPECT_OP(2, string);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(0, integer);

          auto str = L_LOAD_STRING(2);
          auto x = L_LOAD_INT(1);
          auto y = L_LOAD_INT(0);

          if (str_eq(str, "lightning")) {
              if (auto l = APP.alloc_entity<LightningStrike>()) {
                  APP.effects().push(std::move(l));
              }
          } else if (str_eq(str, "explosion")) {
              medium_explosion(
                  {Fixnum::from_integer(x), Fixnum::from_integer(y)});
          } else if (str_eq(str, "big-explosion")) {
              big_explosion({Fixnum::from_integer(x), Fixnum::from_integer(y)});
          }
          return L_NIL;
      }}},
    {"level-retry",
     {SIG0(nil),
      [](int argc) {
          PLATFORM.fill_overlay(0);
          APP.restore_backup();
          PLATFORM.speaker().clear_sounds();
          push_menu_queue.push_back(make_deferred_scene<LoadLevelScene>());
          return L_NIL;
      }}},
    {"savegame",
     {SIG0(nil),
      [](int argc) {
          if (APP.game_mode() == App::GameMode::adventure) {
              if (APP.scene().cast_world_scene()) {
                  // If we're in a level, store our backup save, because, when
                  // storing a regular save, the world map coords would be
                  // wrong. The backup save was created before entering the
                  // level.
                  if (APP.has_backup()) {
                      APP.store_backup();
                      return lisp::make_boolean(true);
                  }
              } else {
                  save::store("", APP.persistent_data());
                  return lisp::make_boolean(true);
              }
          }
          return L_NIL;
      }}},
    {"-decorate-file",
     {SIG1(string, wrapped),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);
          auto p = lisp::get_list(dcompr(lisp::get_op0()->wrapped().data_), 0);
          auto str = p->string().value();
          return lisp::make_string(::format("#(file:%)", str).c_str());
      }}},
    {"file-write!",
     {EMPTY_SIG(3),
      [](int argc) {
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          auto inp_list = dcompr(lisp::get_op(2)->wrapped().data_);

          if (not lisp::is_list(inp_list) or
              not lisp::is_list(lisp::get_op0())) {
              return lisp::make_error("invalid input");
          }

          auto path = lisp::get_list(inp_list, 0);
          auto size = lisp::get_list(inp_list, 1);
          auto data = lisp::get_list(inp_list, 2);

          if (path->type() not_eq lisp::Value::Type::string or
              size->type() not_eq lisp::Value::Type::integer or
              data->type() not_eq lisp::Value::Type::databuffer) {

              return lisp::make_error("invalid input");
          }

          auto len = lisp::length(lisp::get_op0());
          auto offset = L_LOAD_INT(1);

          int size_int = size->integer().value_;
          if (offset == -1) {
              offset = size_int;
          }

          const int newsize = len + offset;

          if (newsize > size_int) {
              size_int = newsize;
          }

          if (newsize > SCRATCH_BUFFER_SIZE) {
              return lisp::make_error("file size exceeds maximum!");
          }

          int i = 0;
          l_foreach(lisp::get_op0(), [&](lisp::Value* v) {
              data->databuffer().value()->data_[offset + i++] =
                  v->integer().value_;
          });

          set_list(inp_list, 1, L_INT(size_int));

          return inp_list;
      }}},
    {"file-store",
     {EMPTY_SIG(1),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);

          auto input = dcompr(lisp::get_op0()->wrapped().data_);

          if (not lisp::is_list(input)) {
              return lisp::make_boolean(false);
          }

          auto path = lisp::get_list(input, 0);
          auto size = lisp::get_list(input, 1);
          auto data = lisp::get_list(input, 2);

          if (path->type() not_eq lisp::Value::Type::string or
              size->type() not_eq lisp::Value::Type::integer or
              data->type() not_eq lisp::Value::Type::databuffer) {

              // TODO: raise error!
              return lisp::make_boolean(false);
          }

          Vector<char> output;
          for (int i = 0; i < size->integer().value_; ++i) {
              output.push_back(data->databuffer().value()->data_[i]);
          }

          auto success = flash_filesystem::store_file_data_binary(
              path->string().value(), output, {.use_compression_ = true});

          return lisp::make_boolean(success);
      }}},
    {"file-unlink",
     {SIG1(nil, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          flash_filesystem::unlink_file(lisp::get_op0()->string().value());
          return L_NIL;
      }}},
    {"file-load",
     {SIG1(wrapped, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          Vector<char> contents;

          lisp::ListBuilder result;
          result.push_back(lisp::get_op0());

          lisp::Protected buf(lisp::make_databuffer("file-contents"));
          memset(buf->databuffer().value()->data_, 0, SCRATCH_BUFFER_SIZE);

          if (APP.load_file(lisp::get_op0()->string().value(), contents)) {

              if (contents.size() > SCRATCH_BUFFER_SIZE) {
                  return lisp::make_error("file too large to load!");
              }

              int i = 0; // NOTE: vector indexing is not as fast as iteration.
              for (char c : contents) {
                  buf->databuffer().value()->data_[i++] = c;
              }

              result.push_back(L_INT(contents.size()));
              result.push_back(buf);

          } else {
              result.push_back(L_INT(0));
              result.push_back(buf);
          }

          return wrap(result.result(), lisp::make_symbol("file"));
      }}},
    {"file-exists?",
     {SIG1(nil, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          auto path = lisp::get_op0()->string().value();
          if (flash_filesystem::file_exists(path)) {
              return lisp::make_boolean(true);
          }

          auto c = PLATFORM.load_file_contents("", path);
          return lisp::make_boolean(c);
      }}},
    {"weather",
     {SIG0(integer), [](int argc) { return L_INT(APP.environment().id()); }}},
    {"weather-set",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          environment_init(L_LOAD_INT(0));
          PLATFORM.screen().set_shader(APP.environment().shader());
          PLATFORM.screen().set_shader_argument(0);

          if (not PLATFORM.speaker().is_music_playing(
                  APP.environment().music()->c_str())) {
              PLATFORM.speaker().stream_music(
                  APP.environment().music()->c_str(), 0);
          }

          APP.player_island().schedule_repaint();

          if (APP.opponent_island()) {
              APP.opponent_island()->schedule_repaint();
          }

          return L_NIL;
      }}},
    {"opponent-reset",
     {SIG0(nil),
      [](int argc) {
          if (not APP.opponent_island()) {
              return L_NIL;
          }
          for (int x = 0; x < 16; ++x) {
              for (int y = 0; y < 16; ++y) {
                  PLATFORM.set_tile(APP.opponent_island()->layer(), x, y, 0);
              }
          }
          APP.reset_opponent_island();
          return L_NIL;
      }}},
    {"opponent-generate",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          auto [app, pfrm] = interp_get_context();

          // Swap in the procedural-generation enemy AI that we created for
          // SKYLAND Forever, generate a level, and then swap the regular Enemy
          // AI back in.
          auto& o = app->swap_opponent<ProcgenEnemyAI>(
              rng::get(rng::critical_state), 1);

          o.set_levelgen_count(lisp::get_op(0)->integer().value_);
          o.generate_level();

          app->swap_opponent<EnemyAI>();

          return L_NIL;
      }}},
    {"opponent-init",
     {SIG2(nil, integer, symbol),
      [](int argc) {
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
      }}},
    {"terrain-set",
     {SIG2(nil, wrapped, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, wrapped);

          auto island = unwrap_isle(lisp::get_op(1));
          island->init_terrain(lisp::get_op(0)->integer().value_);

          return L_NIL;
      }}},
    {"terrain",
     {SIG1(integer, wrapped),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);

          auto island = unwrap_isle(lisp::get_op(0));
          return lisp::make_integer(island->terrain().size());

          return L_NIL;
      }}},
    {"terrain-add-left",
     {SIG1(nil, wrapped),
      [](int argc) {
          auto island = unwrap_isle(lisp::get_op(0));
          island->init_terrain(island->terrain().size() + 1);
          shift_rooms_right(*island);
          return L_NIL;
      }}},
    {"mountain-terrain-mode",
     {SIG2(nil, wrapped, integer),
      [](int argc) {
          auto island = unwrap_isle(lisp::get_op(1));
          island->set_mountain_terrain(L_LOAD_INT(0));
          return L_NIL;
      }}},
    {"room-new",
     {SIG2(nil, wrapped, cons),
      [](int argc) {
          L_EXPECT_OP(0, cons);
          L_EXPECT_OP(1, wrapped);

          auto island = unwrap_isle(lisp::get_op(1));
          auto name = lisp::get_list(lisp::get_op(0), 0)->symbol().name();
          u8 x = lisp::get_list(lisp::get_op(0), 1)->integer().value_;
          u8 y = lisp::get_list(lisp::get_op(0), 2)->integer().value_;

          if (auto c = load_metaclass(name)) {
              (*c)->create(island, RoomCoord{x, y});
              island->repaint();
          } else {
              Platform::fatal(name);
          }

          return L_NIL;
      }}},
    {"room-del",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));

          auto coord = RoomCoord{
              (u8)lisp::get_op(1)->integer().value_,
              (u8)lisp::get_op(0)->integer().value_,
          };

          island->destroy_room(coord);

          return L_NIL;
      }}},
    {"room-load",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));

          auto coord = RoomCoord{
              (u8)lisp::get_op(1)->integer().value_,
              (u8)lisp::get_op(0)->integer().value_,
          };

          if (auto room = island->get_room(coord)) {
              return room->serialize();
          }
          return L_NIL;
      }}},
    {"room-is-critical",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));

          auto coord = RoomCoord{
              (u8)lisp::get_op(1)->integer().value_,
              (u8)lisp::get_op(0)->integer().value_,
          };

          if (auto room = island->get_room(coord)) {
              if ((*room->metaclass())->category() == Room::Category::power) {
                  int pwr_cnt = 0;
                  for (auto& o : island->rooms()) {
                      if ((*o->metaclass())->category() ==
                          Room::Category::power) {
                          ++pwr_cnt;
                      }
                  }
                  if (pwr_cnt == 1) {
                      return lisp::make_boolean(true);
                  }
              }
          }
          return lisp::make_boolean(false);
      }}},
    {"room-mut",
     {SIG4(nil, wrapped, integer, integer, symbol),
      [](int argc) {
          L_EXPECT_OP(0, symbol);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, integer);
          L_EXPECT_OP(3, wrapped);

          auto island = unwrap_isle(lisp::get_op(3));
          auto coord = RoomCoord{
              (u8)lisp::get_op(2)->integer().value_,
              (u8)lisp::get_op(1)->integer().value_,
          };

          if (auto room = island->get_room(coord)) {
              auto tp_name = lisp::get_op(0)->symbol().name();
              room->__unsafe__transmute(metaclass_index(tp_name));
          }

          return L_NIL;
      }}},
    {"chr-del",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer); // y
          L_EXPECT_OP(1, integer); // x
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));

          auto coord = RoomCoord{
              (u8)lisp::get_op(1)->integer().value_,
              (u8)lisp::get_op(0)->integer().value_,
          };

          island->remove_character(coord);

          return L_NIL;
      }}},
    {"chr-id",
     {SIG2(integer, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer); // new-id
          L_EXPECT_OP(1, integer); // old-id

          auto old_id = lisp::get_op(1)->integer().value_;
          auto new_id = lisp::get_op(0)->integer().value_;

          if (auto chr = Character::find_by_id(old_id).first) {
              chr->__assign_id(new_id);
              Character::__rebase_ids(new_id + 1);
          }

          return lisp::get_op(0);
      }}},
    {"chr-hp",
     {EMPTY_SIG(1),
      [](int argc) {
          if (argc == 2) {
              auto hp = lisp::get_op(0)->integer().value_;
              auto id = lisp::get_op(1)->integer().value_;
              if (auto chr = Character::find_by_id(id).first) {
                  chr->__set_health(hp);
                  if (chr->is_replicant()) {
                      chr->set_max_health(hp);
                  }
              }
          } else if (argc == 1) {
              auto id = lisp::get_op(0)->integer().value_;
              if (auto chr = Character::find_by_id(id).first) {
                  return lisp::make_integer(chr->health());
              }
          } else {
              return L_NIL;
          }
          return L_NIL;
      }}},
    {"chr-move",
     {EMPTY_SIG(5),
      [](int argc) {
          L_EXPECT_OP(0, integer); // y2
          L_EXPECT_OP(1, integer); // x2
          L_EXPECT_OP(2, integer); // y1
          L_EXPECT_OP(3, integer); // x1
          L_EXPECT_OP(4, wrapped); // island

          auto island = unwrap_isle(lisp::get_op(4));

          u8 startx = lisp::get_op(3)->integer().value_;
          u8 starty = lisp::get_op(2)->integer().value_;

          u8 destx = lisp::get_op(1)->integer().value_;
          u8 desty = lisp::get_op(0)->integer().value_;

          if (auto room = island->get_room({startx, starty})) {
              for (auto& chr : room->characters()) {
                  if (chr->owner() == &room->parent()->owner()) {

                      auto path = find_path(
                          island, chr.get(), {startx, starty}, {destx, desty});

                      if (path and *path) {
                          chr->set_movement_path(std::move(*path));
                      }

                      break;
                  }
              }
          }
          return L_NIL;
      }}},
    {"chr-new",
     {SIG4(integer, wrapped, integer, integer, symbol),
      [](int argc) {
          L_EXPECT_OP(1, symbol);
          L_EXPECT_OP(2, integer); // y
          L_EXPECT_OP(3, integer); // x
          L_EXPECT_OP(4, wrapped);

          auto island = unwrap_isle(lisp::get_op(4));

          auto coord = RoomCoord{
              (u8)lisp::get_op(3)->integer().value_,
              (u8)lisp::get_op(2)->integer().value_,
          };

          bool is_replicant = false;
          int race = -1;
          int icon = 0;
          u8 kills = 0;
          u8 battles = 0;
          int repaired = 0;
          int step_count = 0;
          u8 fires = 0;

          // For backwards compatibility with old versions. We used to accept a
          // integer parameter indicating whether the character was a
          // replicant. Now we accept an association list of properties. But we
          // still want to load player's save files.
          if (lisp::get_op(0)->type() == lisp::Value::Type::integer) {
              is_replicant = lisp::get_op0()->integer().value_;
          } else {
              // const bool is_replicant = lisp::get_op(0)->integer().value_;
              if (not lisp::is_list(lisp::get_op(0))) {
                  return lisp::make_error(
                      lisp::Error::Code::invalid_argument_type,
                      lisp::make_string("chr-new final arg is not list..."));
              }

              lisp::l_foreach(lisp::get_op(0), [&](lisp::Value* val) {
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
                          if (auto e = find_param("kc")) {
                              kills = e;
                          }
                          if (auto b = find_param("bt")) {
                              battles = b;
                          }
                          if (auto b = find_param("dr")) {
                              repaired = b;
                          }
                          if (auto b = find_param("sc")) {
                              step_count = b;
                          }
                          if (auto b = find_param("fe")) {
                              fires = b;
                          }
                      }
                  }
              });
          }


          s32 id = -1;

          auto conf = lisp::get_op(1);
          if (str_cmp(conf->symbol().name(), "hostile") == 0) {
              APP.swap_opponent<EnemyAI>();
              auto chr = ::skyland::alloc_entity<Character>(
                  island, &APP.opponent(), coord, is_replicant);

              if (chr) {
                  id = chr->id();

                  if (race not_eq -1) {
                      chr->set_race((Character::Race)race);
                  }

                  const auto race = chr->get_race();
                  if (not icon and race == Character::Race::goblin) {
                      static const int count = 4;
                      static const int goblin_icons[count] = {2, 32, 33, 34};
                      int selection = rng::choice(count, rng::utility_state);
                      icon = goblin_icons[selection];
                  }
                  chr->set_icon(icon);

                  island->add_character(std::move(chr));
              }

          } else if (str_cmp(conf->symbol().name(), "neutral") == 0) {
              auto chr = ::skyland::alloc_entity<Character>(
                  island, &APP.player(), coord, is_replicant);

              if (chr) {
                  if (race not_eq -1) {
                      chr->set_race((Character::Race)race);
                  }

                  chr->set_icon(icon);

                  id = chr->id();
                  chr->stats().info_.enemies_vanquished_ = kills;
                  chr->stats().info_.battles_fought_ = battles;
                  chr->stats().info_.damage_repaired_.set(repaired);
                  chr->stats().info_.steps_taken_.set(step_count);
                  chr->stats().info_.fires_extinguished_ = fires;
                  island->add_character(std::move(chr));
              }
          }

          return lisp::make_integer(id);
      }}},
    {"mem-log-diagnostics",
     {SIG0(nil),
      [](int argc) {
          info("__MEMORY_DIAGNOSTICS______");
          scratch_buffer_memory_diagnostics([](const char* line) {
              info(line);
          });
          info(format("extension mem: used %", extension_stats().used));

          info("pool diagnostics:");
          info("        name        |   size  |  total  |  used");
          info("--------------------|---------|---------|--------");
          auto pool = GenericPool::instances();
          while (pool) {
              StringBuffer<96> output;
              output = pool->name();
              auto name_len = strlen(pool->name());
              if (name_len < 20) {
                  output += StringBuffer<20>(' ', 20 - name_len);
              }
              output += "| ";
              auto size_str = stringify(pool->pooled_element_size());
              if (size_str.length() < 7) {
                  output += StringBuffer<7>(' ', 7 - size_str.length());
              }
              output += size_str;
              output += " | ";

              auto elem_count_str = stringify(pool->pooled_element_count());
              if (elem_count_str.length() < 7) {
                  output += StringBuffer<7>(' ', 7 - elem_count_str.length());
              }
              output += elem_count_str;
              output += " | ";

              auto remaining_str = stringify(pool->pooled_element_count() -
                                             pool->pooled_element_remaining());
              if (remaining_str.length() < 7) {
                  output += StringBuffer<7>(' ', 7 - remaining_str.length());
              }
              output += remaining_str;

              info(output);


              pool = pool->next();
          }
          return L_NIL;
      }}},
    {"click",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          RoomCoord coord;
          coord.x = lisp::get_op(1)->integer().value_;
          coord.y = lisp::get_op(0)->integer().value_;

          if (auto room = (unwrap_isle(lisp::get_op(2)))->get_room(coord)) {
              room->select(coord);
          }

          return L_NIL;
      }}},
    {"sel-move",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, wrapped);

          RoomCoord& sel = globals().near_cursor_loc_;

          if (auto ws = APP.scene().cast_world_scene()) {
              if (unwrap_isle(lisp::get_op(2)) == &APP.player_island()) {
                  sel = globals().far_cursor_loc_;
                  ws->near_camera();
              } else {
                  ws->far_camera();
              }

              sel.x = lisp::get_op(1)->integer().value_;
              sel.y = lisp::get_op(0)->integer().value_;
          }
          return L_NIL;
      }}},
    {"sel-input-opponent",
     {SIG3(nil, nil, string, function),
      [](int argc) {
          L_EXPECT_OP(0, function);
          L_EXPECT_OP(1, string);

          auto bundle = lisp::make_cons(lisp::get_op(1), lisp::get_op(0));
          bundle = lisp::make_cons(bundle, lisp::get_op(2));
          bundle = lisp::make_cons(L_INT(0), bundle);
          if (bundle->type() == lisp::Value::Type::cons) {
              APP.setup_input(bundle);
          }

          return L_NIL;
      }}},
    {"sel-input",
     {SIG3(nil, nil, string, function),
      [](int argc) {
          L_EXPECT_OP(0, function);
          L_EXPECT_OP(1, string);

          auto bundle = lisp::make_cons(lisp::get_op(1), lisp::get_op(0));
          bundle = lisp::make_cons(bundle, lisp::get_op(2));
          bundle = lisp::make_cons(L_INT(1), bundle);
          if (bundle->type() == lisp::Value::Type::cons) {
              APP.setup_input(bundle);
          }

          return L_NIL;
      }}},
    {"island-set-pos",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(2, wrapped);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(0, integer);
          auto island = unwrap_isle(lisp::get_op(2));
          if (island == APP.opponent_island()) {
              island->set_drift(Fixnum(-0.000025f));
          }

          island->set_position({Fixnum::from_integer(L_LOAD_INT(1)),
                                Fixnum::from_integer(L_LOAD_INT(0))});
          return L_NIL;
      }}},
    {"island-pos",
     {SIG1(cons, wrapped),
      [](int argc) {
          L_EXPECT_OP(0, wrapped);
          auto island = unwrap_isle(lisp::get_op(1));
          return L_CONS(L_INT(island->visual_origin().x.as_integer()),
                        L_INT(island->visual_origin().y.as_integer()));
      }}},
    {"island-configure",
     {SIG2(nil, wrapped, cons),
      [](int argc) {
          L_EXPECT_OP(0, cons);
          L_EXPECT_OP(1, wrapped);

          auto island = unwrap_isle(lisp::get_op(1));

          configure_island(*island, lisp::get_op(0));

          return L_NIL;
      }}},
    {"flag-show",
     {SIG2(nil, wrapped, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, wrapped);

          auto island = unwrap_isle(lisp::get_op(1));
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
              show_island_interior(island);
          } else {
              show_island_exterior(island);
          }

          return L_NIL;
      }}},
    {"exit",
     {SIG0(nil),
      [](int argc) {
          if (argc == 1) {
              L_EXPECT_OP(0, integer);
              APP.exit_condition() = (App::ExitCondition)L_LOAD_INT(0);
          } else {
              APP.exit_condition() = App::ExitCondition::misc;
          }
          return L_NIL;
      }}},
    {"coins",
     {SIG0(integer),
      [](int argc) {
          if (APP.macrocosm()) {
              // return lisp::make_integer(macrocosm(*app).data_->p().coins_.get());
              return L_NIL;
          } else {
              return lisp::make_integer(APP.coins());
          }
      }}},
    {"coins-add",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          if (APP.macrocosm()) {
              // auto current = macrocosm(*app).data_->p().coins_.get();
              // current += L_LOAD_INT(0);
              // macrocosm(*app).data_->p().coins_.set(
              //     std::min(std::numeric_limits<macro::Coins>::max(), current));
          } else {
              APP.set_coins(std::max(0, (int)(L_LOAD_INT(0) + APP.coins())));
          }

          return L_NIL;
      }}},
    {"coins-set",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          if (APP.macrocosm()) {
              // auto val = L_LOAD_INT(0);
              // macrocosm(*app).data_->p().coins_.set(
              //     std::min(std::numeric_limits<macro::Coins>::max(), val));
          } else {
              APP.set_coins(L_LOAD_INT(0));
          }

          return L_NIL;
      }}},
    {"coins-victory",
     {SIG0(integer), [](int argc) { return L_INT(APP.victory_coins()); }}},
    {"lint-file",
     {SIG1(nil, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          auto path = lisp::get_op(0)->string().value();

          if (auto contents = PLATFORM.load_file_contents("", path)) {
              lisp::BasicCharSequence seq(contents);
              auto result = lisp::lint_code(seq);
              return result;
          } else {
              StringBuffer<100> err("script '");
              err += path;
              err += "' missing";
              PLATFORM.fatal(err.c_str());
          }
      }}},
    {"eval-file",
     {SIG1(nil, string),
      [](int argc) {
          L_EXPECT_OP(0, string);

          auto str = lisp::get_op(0)->string().value();

          return APP.invoke_script(str);
      }}},
    {"choice",
     {SIG1(integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          return lisp::make_integer(rng::choice(
              lisp::get_op(0)->integer().value_, rng::critical_state));
      }}},
    {"autopilot",
     {SIG1(nil, cons),
      [](int argc) {
          L_EXPECT_OP(0, cons);

          APP.swap_player<AutopilotPlayer>(lisp::get_op(0));

          return L_NIL;
      }}},
    {"read-ini",
     {SIG3(nil, string, string, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          L_EXPECT_OP(1, string);
          L_EXPECT_OP(2, string);

          auto f = PLATFORM.load_file_contents("", L_LOAD_STRING(2));
          if (f) {
              Conf c;
              auto res = c.get(f, L_LOAD_STRING(1), L_LOAD_STRING(0));
              if (auto val = std::get_if<Conf::String>(&res)) {
                  return lisp::make_string((*val)->c_str());
              }
          }

          return L_NIL;
      }}},
    {"filesystem-remaining",
     {SIG0(integer),
      [](int argc) {
          return L_INT(flash_filesystem::statistics().bytes_available_);
      }}},
    {"filesystem-walk",
     {SIG2(nil, string, function),
      [](int argc) {
          L_EXPECT_OP(1, string);
          L_EXPECT_OP(0, function);

          auto search = lisp::get_op(1);
          auto fn = lisp::get_op(0);

          PLATFORM.walk_filesystem([fn, search](const char* path) {
              auto len = strlen(search->string().value());
              for (u32 i = 0; i < len; ++i) {
                  if (path[i] not_eq search->string().value()[i]) {
                      return;
                  }
              }

              lisp::push_op(lisp::make_string(path));
              lisp::funcall(fn, 1);
              lisp::pop_op(); // discard funcall result
          });

          return L_NIL;
      }}},
    {"file-line-count",
     {SIG1(integer, string),
      [](int argc) {
          L_EXPECT_OP(0, string);
          auto fname = lisp::get_op(0)->string().value();
          return L_INT(file_line_count(fname));
      }}},
    {"file-get-line",
     {SIG2(nil, string, integer),
      [](int argc) {
          L_EXPECT_OP(1, string);
          L_EXPECT_OP(0, integer);

          auto line = get_line_from_file(lisp::get_op(1)->string().value(),
                                         lisp::get_op(0)->integer().value_);

          if (line) {
              return lisp::make_string(line->c_str());
          }

          return L_NIL;
      }}},
    {"configure-rooms",
     {SIG1(nil, cons),
      [](int argc) {
          L_EXPECT_OP(0, cons);

          lisp::l_foreach(lisp::get_op(0), [](lisp::Value* val) {
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
      }}},
    {"cargo",
     {SIG3(nil, wrapped, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer); // y
          L_EXPECT_OP(1, integer); // x
          L_EXPECT_OP(2, wrapped);

          auto island = unwrap_isle(lisp::get_op(2));
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
      }}},
    {"faction",
     {SIG0(symbol),
      [](int argc) {
          switch (APP.faction()) {
          default:
          case Faction::human:
              return L_SYM("human");
          case Faction::sylph:
              return L_SYM("sylph");
          case Faction::goblin:
              return L_SYM("goblin");
          }
      }}},
    {"faction-set",
     {SIG1(nil, symbol),
      [](int argc) {
          auto sym = lisp::get_op0()->symbol().name();
          Faction f;
          if (str_eq(sym, "human")) {
              f = Faction::human;
          } else if (str_eq(sym, "sylph")) {
              f = Faction::sylph;
          } else if (str_eq(sym, "goblin")) {
              f = Faction::goblin;
          } else {
              return lisp::make_error("invalid faction symbol!");
          }
          APP.faction() = f;
          return L_NIL;
      }}},
    {"cargo-set",
     {SIG4(nil, wrapped, integer, integer, string),
      [](int argc) {
          L_EXPECT_OP(0, string);  // cargo
          L_EXPECT_OP(1, integer); // y
          L_EXPECT_OP(2, integer); // x
          L_EXPECT_OP(3, wrapped);

          auto island = unwrap_isle(lisp::get_op(3));
          const u8 x = lisp::get_op(2)->integer().value_;
          const u8 y = lisp::get_op(1)->integer().value_;

          if (auto room = island->get_room({x, y})) {
              if (auto cb = room->cast<CargoBay>()) {
                  cb->set_cargo(lisp::get_op(0)->string().value(), 1);
              }
          }

          return L_NIL;
      }}},
    {"qr-set",
     {SIG4(nil, wrapped, integer, integer, string),
      [](int argc) {
          L_EXPECT_OP(0, string);  // cargo
          L_EXPECT_OP(1, integer); // y
          L_EXPECT_OP(2, integer); // x
          L_EXPECT_OP(3, wrapped);

          auto island = unwrap_isle(lisp::get_op(3));
          const u8 x = lisp::get_op(2)->integer().value_;
          const u8 y = lisp::get_op(1)->integer().value_;

          if (auto room = island->get_room({x, y})) {
              if (auto cb = room->cast<QrBlock>()) {
                  cb->set_message(lisp::get_op(0)->string().value());
              }
          }

          return L_NIL;
      }}},
    {"achieve",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          const auto achievement =
              (achievements::Achievement)(lisp::get_op(0)->integer().value_);

          if ((int)achievement >= (int)achievements::Achievement::count) {
              return L_NIL;
          }

          achievements::raise(achievement);

          return L_NIL;
      }}},
    {"wg-generate",
     {SIG0(nil),
      [](int argc) {
          APP.world_graph().generate();
          return L_NIL;
      }}},
    {"wg-path",
     {SIG2(nil, cons, cons),
      [](int argc) {
          L_EXPECT_OP(0, cons);
          L_EXPECT_OP(1, cons);

          s8 x1 = lisp::get_op1()->cons().car()->integer().value_;
          s8 y1 = lisp::get_op1()->cons().cdr()->integer().value_;

          s8 x2 = lisp::get_op0()->cons().car()->integer().value_;
          s8 y2 = lisp::get_op0()->cons().cdr()->integer().value_;

          auto path = APP.world_graph().path({x1, y1}, {x2, y2});

          lisp::ListBuilder path_list;
          for (auto& n : path) {
              path_list.push_front(L_CONS(L_INT(n.x), L_INT(n.y)));
          }

          return path_list.result();
      }}},
    {"wg-nav-path",
     {SIG0(cons),
      [](int argc) {
          lisp::ListBuilder list;
          for (auto i : WorldMapScene::nav_path()) {
              list.push_back(L_INT(i));
          }
          return list.result();
      }}},
    {"wg-nav-path-set",
     {SIG1(nil, cons),
      [](int argc) {
          auto list = lisp::get_op0();
          if (not lisp::is_list(list)) {
              return lisp::make_error("invalid input");
          }
          WorldMapScene::nav_path().clear();
          lisp::l_foreach(list, [](lisp::Value* v) {
              WorldMapScene::nav_path().push_back(v->integer().value_);
          });
          return L_NIL;
      }}},
    {"wg-current-type",
     {SIG0(integer),
      [](int argc) {
          const auto loc = APP.current_world_location();
          return L_INT((int)APP.world_graph().nodes_[loc].type_);
      }}},
    {"wg-nodes",
     {SIG0(cons),
      [](int argc) {
          lisp::ListBuilder builder;
          for (auto& node : APP.world_graph().nodes_) {
              if (node.type_ not_eq WorldGraph::Node::Type::null) {
                  builder.push_back(L_CONS(
                      L_INT((int)node.type_),
                      L_CONS(L_INT(node.coord_.x), L_INT(node.coord_.y))));
              }
          }
          return builder.result();
      }}},
    {"wg-node-set",
     {SIG3(nil, integer, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, integer);

          const s8 x = L_LOAD_U8(2);
          const s8 y = L_LOAD_U8(1);

          for (auto& node : APP.world_graph().nodes_) {
              if (node.coord_ == Vec2<s8>{x, y}) {
                  node.type_ = (WorldGraph::Node::Type)L_LOAD_U8(0);
                  break;
              }
          }

          return L_NIL;
      }}},
    {"wg-storm-frontier",
     {SIG0(integer),
      [](int argc) { return L_INT(APP.world_graph().storm_depth_); }}},
    {"wg-turns-remaining",
     {SIG1(integer, cons),
      [](int argc) {
          L_EXPECT_OP(0, cons);
          int x = lisp::get_op0()->cons().car()->integer().value_;
          int turns = 0;
          while (not is_x_behind_storm_frontier(x, turns)) {
              ++turns;
          }
          return L_INT(turns);
      }}},
    {"wg-is-x-behind-frontier?",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          int x = L_LOAD_INT(0);
          return lisp::make_boolean(is_x_behind_storm_frontier(x, 0));
      }}},
    {"wg-storm-frontier-set",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          for (auto& node : APP.world_graph().nodes_) {
              if (node.type_ == WorldGraph::Node::Type::corrupted and
                  is_x_behind_storm_frontier(node.coord_.x, 0)) {
                  node.type_ = WorldGraph::Node::Type::visited;
              }
          }
          APP.world_graph().storm_depth_ = L_LOAD_INT(0);
          return L_NIL;
      }}},
    {"wg-pos",
     {SIG0(cons),
      [](int argc) {
          const auto& node =
              APP.world_graph().nodes_[APP.current_world_location()];

          return L_CONS(L_INT(APP.zone() - 1),
                        L_CONS(L_INT(node.coord_.x), L_INT(node.coord_.y)));
      }}},
    {"gui-add-node",
     {EMPTY_SIG(2),
      [](int argc) {
          L_EXPECT_OP(0, string);
          L_EXPECT_OP(1, string);
          APP.scene().gui_add_node(nullptr, L_LOAD_STRING(1), L_LOAD_STRING(0));
          return L_NIL;
      }}},
    {"gui-delete-node",
     {EMPTY_SIG(1),
      [](int argc) {
          L_EXPECT_OP(0, string);
          APP.scene().gui_delete_node(L_LOAD_STRING(0));
          return L_NIL;
      }}},
    {"fatal",
     {EMPTY_SIG(1),
      [](int argc) {
          lisp::DefaultPrinter p;
          format(lisp::get_op(0), p);
          Platform::fatal(p.data_.c_str());

          return L_NIL;
      }}},
    {"gui-set-attr",
     {EMPTY_SIG(3),
      [](int argc) {
          L_EXPECT_OP(1, string);
          L_EXPECT_OP(2, string);
          APP.scene().gui_set_attr(
              L_LOAD_STRING(2), L_LOAD_STRING(1), lisp::get_op0());
          return L_NIL;
      }}},
    {"construction-sites",
     {SIG2(cons, wrapped, cons),
      [](int argc) {
          L_EXPECT_OP(1, wrapped);
          L_EXPECT_OP(0, cons);

          const int sx = lisp::get_op(0)->cons().car()->integer().value_;
          const int sy = lisp::get_op(0)->cons().cdr()->integer().value_;

          auto island = unwrap_isle(lisp::get_op(1));

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
      }}},
    {"databuffer-inspect",
     {SIG1(nil, databuffer),
      [](int argc) {
          L_EXPECT_OP(0, databuffer);
          auto sbr = lisp::get_op0()->databuffer().value();

          push_menu_queue.emplace_back([sbr] {
              auto next = make_scene<HexViewerModule>(sbr);
              next->next_scene_ = make_deferred_scene<ReadyScene>();
              return next;
          });
          return L_NIL;
      }}},
    {"setvar",
     {SIG2(nil, string, integer),
      [](int argc) {
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
      }}},
    {"getvar",
     {SIG1(integer, string),
      [](int argc) {
          L_EXPECT_OP(0, string);

          if (auto v =
                  SharedVariable::load(lisp::get_op(0)->string().value())) {
              return lisp::make_integer(v->get());
          }

          StringBuffer<96> error("access to invalid shared variable '");
          error += lisp::get_op(0)->string().value();
          error += "'";

          Platform::fatal(error.c_str());
      }}},
    {"mem-sbr-used",
     {SIG0(integer), [](int argc) { return L_INT(scratch_buffers_in_use()); }}},
    {"mem-sbr-free",
     {SIG0(integer),
      [](int argc) { return L_INT(scratch_buffers_remaining()); }}},
    {"mem-pools",
     {SIG0(cons),
      [](int argc) {
          lisp::ListBuilder list;
          auto p = GenericPool::instances();
          while (p) {
              list.push_back(lisp::make_symbol(p->name()));
              p = p->next();
          }
          return list.result();
      }}},
    {"mem-pool-info",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          lisp::ListBuilder list;
          auto p = GenericPool::instances();
          int i = 0;
          while (p) {
              if (i == L_LOAD_INT(0)) {
                  auto stat = [&](const char* l, int v) {
                      list.push_back(L_CONS(L_SYM(l), L_INT(v)));
                  };

                  list.push_back(lisp::make_symbol(p->name()));
                  stat("size", p->pooled_element_size());
                  stat("count", p->pooled_element_count());
                  stat("used",
                       p->pooled_element_count() -
                           p->pooled_element_remaining());
                  return list.result();
              }
              p = p->next();
              ++i;
          }
          return L_NIL;
      }}},
    {"save-bit-load",
     {SIG1(integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          return L_INT(APP.gp_.stateflags_.get(L_LOAD_INT(0)));
      }}},
    {"save-bit-store",
     {SIG2(integer, integer, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);

          auto prev = APP.gp_.stateflags_.get(L_LOAD_INT(1));

          const bool was_set = APP.gp_.stateflags_.get(L_LOAD_INT(1));
          if (not was_set) {
              APP.gp_.stateflags_.set(L_LOAD_INT(1), L_LOAD_INT(0));
              save::store_global_data(APP.gp_);
          }

          return L_INT(prev);
      }}},
    {"recover-crash",
     {SIG0(nil),
      [](int argc) {
          flash_filesystem::copy_file("/crash/adventure.dat",
                                      "/save/adventure.dat");

          flash_filesystem::copy_file("/crash/adventure.lisp",
                                      "/save/adventure.lisp");

          PLATFORM.fill_overlay(0);
          PLATFORM.speaker().clear_sounds();
          PLATFORM.speaker().stream_music("unaccompanied_wind.raw", 0);
          APP.game_mode() = App::GameMode::adventure;

          push_menu_queue.push_back(make_deferred_scene<StartAdventureScene>());

          return L_NIL;
      }}},
    {"device-info",
     {SIG1(string, symbol),
      [](int argc) {
          auto sym = lisp::get_op(0)->symbol();
          if (str_eq(sym.name(), "name")) {
              return lisp::make_string(PLATFORM.device_name().c_str());
          } else if (str_eq(sym.name(), "variant")) {
              return lisp::make_string(PLATFORM.model_name().c_str());
          }
          return L_NIL;
      }}},
    {"is-developer-mode",
     {SIG0(nil),
      [](int argc) { return lisp::make_boolean(APP.is_developer_mode()); }}},
    {"is-regression-mode",
     {SIG0(nil),
      [](int argc) {
          return lisp::make_boolean(state_bit_load(StateBit::regression));
      }}},
    {"challenge-complete",
     {SIG1(nil, integer),
      [](int argc) {
          L_EXPECT_OP(0, integer);

          int challenge = lisp::get_op(0)->integer().value_;
          u64 challenge_bitmask = 1 << challenge;

          const bool was_set =
              APP.gp_.challenge_flags_.get() & challenge_bitmask;

          if (not was_set) {
              APP.gp_.challenge_flags_.set(APP.gp_.challenge_flags_.get() |
                                           challenge_bitmask);

              save::store_global_data(APP.gp_);
          }


          return L_NIL;
      }}},
});



static Binding binding_lookup_function(const char* name)
{
    auto found_binding = binding_table.find(name);
    if (found_binding not_eq binding_table.end()) {
        return found_binding->second;
    }

    return {EMPTY_SIG(0), nullptr};
}



static const char* binding_lookup_name(lisp::NativeInterface::Function f)
{
    for (auto& kvp : binding_table) {
        if (kvp.second.second == f) {
            return kvp.first.c_str();
        }
    }

    return nullptr;
}



static void binding_name_getter(lisp::SymbolCallback cb)
{
    for (auto& fn : binding_table) {
        cb(fn.first.c_str());
    }

    for (auto& sym : symtab) {
        cb(sym.first.c_str());
    }

    auto [mt, ms] = room_metatable();

    for (int i = 0; i < plugin_rooms_begin(); ++i) {
        cb(mt[i]->name());
    }
}



static const char* binding_intern_sym_resolver(const char* name)
{
    auto found_intern = binding_table.find(name);
    if (found_intern not_eq binding_table.end()) {
        return found_intern->first.c_str();
    }

    auto found_sym = symtab.find(name);
    if (found_sym not_eq symtab.end()) {
        return found_sym->first.c_str();
    }

    if (auto mt = load_metaclass(name)) {
        return (*mt)->name();
    }

    return nullptr;
}



void App::init_scripts(Function<4 * sizeof(void*), void(const char*)> msg)
{
    msg("lisp init...");

    lisp::init(PLATFORM.load_file("", "/lisp_symtab.dat"),
               PLATFORM.load_file("", "/lisp_constant_tab.dat"));

    msg("export api...");

    lisp::NativeInterface ni;
    ni.resolve_intern_sym_ = binding_intern_sym_resolver;
    ni.lookup_function_ = binding_lookup_function;
    ni.lookup_name_ = binding_lookup_name;
    ni.get_symbols_ = binding_name_getter;

    lisp::register_native_interface(ni);

    // NOTE: we need to disable custom scripts during startup, otherwise,
    // someone could irreversibly mess up a game.
    const bool was_developer_mode = is_developer_mode();
    set_developer_mode(false);

    auto log_cnt = [&] {
        msg(format("loading LISP fns... (%)", lisp::toplevel_count()).c_str());
    };

    log_cnt();

    invoke_script("/scripts/stdlib.lisp", true);

    log_cnt();

    invoke_script("/scripts/init.lisp", true);

    log_cnt();


    set_developer_mode(was_developer_mode);
}



} // namespace skyland
