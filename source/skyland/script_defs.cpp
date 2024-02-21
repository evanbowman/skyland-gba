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
#include "scene/modules/fileBrowserModule.hpp"
#include "scene/qrViewerScene.hpp"
#include "scene/readyScene.hpp"
#include "scene/scriptHookScene.hpp"
#include "scene/scriptedMenuScene.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "serial.hpp"
#include "sharedVariable.hpp"
#include "skyland.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/misc/lightningStrike.hpp"
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



extern Sound cannon_sound;
extern Sound missile_sound;



bool is_x_behind_storm_frontier(int x, int storm_offset);



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


BINDING_TABLE({
    {"player",
     {0, [](int argc) { return lisp::make_userdata(&APP.player_island()); }}},
    {"opponent",
     {0,
      [](int argc) {
          if (not APP.opponent_island()) {
              if (auto pfrm = lisp::interp_get_pfrm()) {
                  pfrm->fatal("opponent unassigned");
              }
          }
          return lisp::make_userdata(APP.opponent_island());
      }}},
    {"lang-set",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, string);
          systemstring_bind_file(L_LOAD_STRING(0));
          return L_NIL;
      }}},
    {"lang",
     {0,
      [](int argc) { return lisp::make_string(systemstring_bound_file()); }}},
    {"log",
     {1,
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
    {"groups",
     {0,
      [](int argc) {
          lisp::ListBuilder ret;

          lisp::ListBuilder up;
          lisp::ListBuilder left;
          lisp::ListBuilder right;

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
          }

          ret.push_back(L_CONS(L_SYM("Up"), up.result()));
          ret.push_back(L_CONS(L_SYM("Left"), left.result()));
          ret.push_back(L_CONS(L_SYM("Right"), right.result()));

          return ret.result();
      }}},
    {"groups-reset",
     {0,
      [](int argc) {
          for (auto& room : APP.player_island().rooms()) {
              room->set_group(Room::Group::none);
          }
          return L_NIL;
      }}},
    {"emit",
     {5,
      [](int argc) {
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

          if (auto room = island->get_room({x1, y1})) {
              if ((*room->metaclass())->category() == Room::Category::weapon) {
                  room->set_target({x2, y2}, false);
                  if (not str_eq(room->name(), "decimator")) {
                      ((Weapon*)room)->fire();
                  }
                  room->unset_target();
              }
          }

          return L_NIL;
      }}},
    {"groups-add",
     {3,
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
     {1,
      [](int argc) {
          L_EXPECT_OP(0, string);
          lisp::interp_get_pfrm()->speaker().play_sound(L_LOAD_STRING(0), 6);
          return L_NIL;
      }}},
    {"difficulty",
     {0, [](int argc) { return L_INT((int)APP.gp_.difficulty_); }}},
    {"difficulty-set",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          APP.gp_.difficulty_ = (GlobalPersistentData::Difficulty)L_LOAD_INT(0);
          return L_NIL;
      }}},
    {"mcr-block-set",
     {4,
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
    {"help",
     {0,
      [](int argc) {
          using scene_pool::make_deferred_scene;

          if (argc == 0) {
              push_menu_queue.emplace_back([] {
                  UserContext ctx;
                  ctx.browser_exit_scene_ = make_deferred_scene<ReadyScene>();
                  return scene_pool::alloc<FileBrowserModule>(
                      std::move(ctx), "/help/", true);
              });
          }
          return L_NIL;
      }}},
    {"push-menu",
     {2,
      [](int argc) {
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
      }}},
    {"mcr-block",
     {3,
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
     {0,
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
     {1,
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
     {0,
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
    {"rcnt",
     {2,
      [](int argc) {
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
      }}},
    {"room-meta",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, symbol);

          auto mt = load_metaclass(lisp::get_op(0)->symbol().name());

          lisp::ListBuilder b;
          b.push_back(L_CONS(L_SYM("name"),
                             lisp::make_string((*mt)->ui_name()->c_str())));

          auto sz = (*mt)->size();

          b.push_back(L_CONS(L_SYM("size"), L_CONS(L_INT(sz.x), L_INT(sz.y))));
          b.push_back(L_CONS(L_SYM("ico1"), L_INT((*mt)->icon())));
          b.push_back(L_CONS(L_SYM("ico2"), L_INT((*mt)->unsel_icon())));
          b.push_back(L_CONS(L_SYM("pwr"), L_INT((*mt)->consumes_power())));
          b.push_back(L_CONS(L_SYM("cost"), L_INT((*mt)->cost())));

          return b.result();
      }}},
    {"cart-add",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);

          DataCartLibrary lib;
          lib.store(L_LOAD_INT(0));

          return L_NIL;
      }}},
    {"cart-found?",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);

          DataCartLibrary lib;
          if (lib.load(L_LOAD_INT(0))) {
              return L_INT(1);
          }

          return L_NIL;
      }}},
    {"cart-read",
     {2,
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
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);

          DataCart cart(L_LOAD_INT(0));
          lisp::ListBuilder list;

          list.push_back(lisp::make_string(cart.name().c_str()));
          list.push_back(lisp::make_string(cart.subheading().c_str()));

          return list.result();
      }}},
    {"show-fps",
     {1,
      [](int argc) {
          state_bit_store(StateBit::show_fps,
                          lisp::is_boolean_true(lisp::get_op0()));
          return L_NIL;
      }}},
    {"key-bind",
     {2,
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
     {0,
      [](int argc) {
          APP.key_callback_processor().clear();
          return L_NIL;
      }}},
    {"has-dialog?",
     {0,
      [](int argc) { return lisp::make_boolean((bool)APP.dialog_buffer()); }}},
    {"dialog",
     {1,
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
     {3,
      [](int argc) {
          L_EXPECT_OP(0, integer); // y
          L_EXPECT_OP(1, integer); // x
          L_EXPECT_OP(2, user_data);

          auto island = (Island*)lisp::get_op(2)->user_data().obj_;

          const u8 x = L_LOAD_INT(1);
          const u8 y = L_LOAD_INT(0);

          island->fire_create({x, y});

          return L_NIL;
      }}},
    {"version",
     {0,
      [](int argc) {
          lisp::ListBuilder result;
          result.push_back(lisp::make_integer(PROGRAM_MAJOR_VERSION));
          result.push_back(lisp::make_integer(PROGRAM_MINOR_VERSION));
          result.push_back(lisp::make_integer(PROGRAM_SUBMINOR_VERSION));
          result.push_back(lisp::make_integer(PROGRAM_VERSION_REVISION));

          return result.result();
      }}},
    {"rooms",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, user_data);

          auto island = (Island*)lisp::get_op(0)->user_data().obj_;

          lisp::ListBuilder result;
          for (auto& room : island->rooms()) {
              lisp::push_op(room->serialize()); // protect from gc.
              result.push_back(lisp::get_op(0));
              lisp::pop_op();
          }

          return result.result();
      }}},
    {"fs-file-size",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, string);
          Vector<char> data;
          if (APP.load_file(L_LOAD_STRING(0), data)) {
              return L_INT(data.size());
          }
          return L_INT(0);
      }}},
    {"fs-file-read",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, string);
          Vector<char> data;
          if (APP.load_file(L_LOAD_STRING(0), data)) {
              lisp::ListBuilder list;
              auto temp = allocate_dynamic<StringBuffer<2000>>("str-buf");
              for (char c : data) {
                  if (temp->full()) {
                      list.push_back(lisp::make_string(temp->c_str()));
                      temp->clear();
                  }
                  temp->push_back(c);
              }
              if (not temp->empty()) {
                  list.push_back(lisp::make_string(temp->c_str()));
              }
              return list.result();
          }
          return L_NIL;
      }}},
    {"fs-file-write",
     {2,
      [](int argc) {
          L_EXPECT_OP(0, cons);
          L_EXPECT_OP(1, string);

          Vector<char> data;
          lisp::foreach (lisp::get_op0(), [&](lisp::Value* v) {
              if (v->type() == lisp::Value::Type::string) {
                  auto str = v->string().value();
                  while (*str not_eq '\0') {
                      data.push_back(*(str++));
                  }
              }
          });

          auto written =
              flash_filesystem::store_file_data_binary(L_LOAD_STRING(1), data);
          return lisp::make_boolean(written);
      }}},
    {"fs-readdir",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, string);

          lisp::ListBuilder list;

          flash_filesystem::walk_directory(
              L_LOAD_STRING(0), [&](const char* path) {
                  list.push_back(lisp::make_string(path));
              });

          PLATFORM.walk_filesystem([&](const char* path) {
              StringBuffer<64> prefix(L_LOAD_STRING(0));
              if (starts_with(prefix.c_str(), StringBuffer<64>(path))) {
                  list.push_back(lisp::make_string(path));
              }
          });

          return list.result();
      }}},
    {"fs-avail",
     {0,
      [](int argc) {
          auto stat = flash_filesystem::statistics();
          return L_INT(stat.bytes_available_);
      }}},
    {"chrs",
     {1,
      [](int argc) {
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
                          chr_info.push_back(make_cons_safe(
                              make_symbol("race"), make_integer(race)));
                      }
                      if (auto icon = chr->get_icon()) {
                          chr_info.push_back(make_cons_safe(
                              make_symbol("icon"), make_integer(icon)));
                      }
                      chr_info.push_back(make_cons(make_symbol("id"),
                                                   make_integer(chr->id())));
                      list.push_front(chr_info.result());
                  }
              }
          }
          return list.result();
      }}},
    {"chr-slots",
     {0,
      [](int argc) {
          L_EXPECT_OP(0, user_data);

          bool matrix[16][16];

          auto island = (Island*)lisp::get_op(0)->user_data().obj_;

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
     {1,
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
     {2,
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
     {1,
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
     {3,
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
    {"weather",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);

          environment_init(L_LOAD_INT(0));
          PLATFORM.screen().set_shader(APP.environment().shader());
          PLATFORM.screen().set_shader_argument(0);

          if (not PLATFORM.speaker().is_music_playing(
                  APP.environment().music())) {
              PLATFORM.speaker().play_music(APP.environment().music(), 0);
          }

          return L_NIL;
      }}},
    {"opponent-reset",
     {0,
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
     {1,
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
     {2,
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
     {2,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, user_data);

          auto island = (Island*)lisp::get_op(1)->user_data().obj_;
          island->init_terrain(lisp::get_op(0)->integer().value_);

          return L_NIL;
      }}},
    {"terrain",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, user_data);

          auto island = (Island*)lisp::get_op(0)->user_data().obj_;
          return lisp::make_integer(island->terrain().size());

          return L_NIL;
      }}},
    {"room-new",
     {2,
      [](int argc) {
          L_EXPECT_OP(0, cons);
          L_EXPECT_OP(1, user_data);

          auto island = (Island*)lisp::get_op(1)->user_data().obj_;
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
     {3,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, user_data);

          auto island = (Island*)lisp::get_op(2)->user_data().obj_;

          auto coord = RoomCoord{
              (u8)lisp::get_op(1)->integer().value_,
              (u8)lisp::get_op(0)->integer().value_,
          };

          island->destroy_room(coord);

          return L_NIL;
      }}},
    {"room-load",
     {3,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, user_data);

          auto island = (Island*)lisp::get_op(2)->user_data().obj_;

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
     {3,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, user_data);

          auto island = (Island*)lisp::get_op(2)->user_data().obj_;

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
     {4,
      [](int argc) {
          L_EXPECT_OP(0, symbol);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, integer);
          L_EXPECT_OP(3, user_data);

          auto island = (Island*)lisp::get_op(3)->user_data().obj_;
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
     {3,
      [](int argc) {
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
      }}},
    {"chr-id",
     {2,
      [](int argc) {
          L_EXPECT_OP(0, integer); // new-id
          L_EXPECT_OP(1, integer); // old-id

          auto old_id = lisp::get_op(1)->integer().value_;
          auto new_id = lisp::get_op(0)->integer().value_;

          if (auto chr = BasicCharacter::find_by_id(old_id).first) {
              chr->__assign_id(new_id);
              BasicCharacter::__rebase_ids(new_id + 1);
          }

          return lisp::get_op(0);
      }}},
    {"chr-hp",
     {1,
      [](int argc) {
          if (argc == 2) {
              auto hp = lisp::get_op(0)->integer().value_;
              auto id = lisp::get_op(1)->integer().value_;
              if (auto chr = BasicCharacter::find_by_id(id).first) {
                  chr->__set_health(hp);
              }
          } else if (argc == 1) {
              auto id = lisp::get_op(0)->integer().value_;
              if (auto chr = BasicCharacter::find_by_id(id).first) {
                  return lisp::make_integer(chr->health());
              }
          } else {
              return L_NIL;
          }
          return L_NIL;
      }}},
    {"chr-move",
     {5,
      [](int argc) {
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
     {5,
      [](int argc) {
          L_EXPECT_OP(1, symbol);
          L_EXPECT_OP(2, integer); // y
          L_EXPECT_OP(3, integer); // x
          L_EXPECT_OP(4, user_data);

          auto island = (Island*)lisp::get_op(4)->user_data().obj_;

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
                  return lisp::make_error(
                      lisp::Error::Code::invalid_argument_type,
                      lisp::make_string("chr-new final arg is not list..."));
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
              APP.swap_opponent<EnemyAI>();
              auto chr = ::skyland::alloc_entity<BasicCharacter>(
                  island, &APP.opponent(), coord, is_replicant);

              if (chr) {
                  id = chr->id();

                  chr->set_icon(icon);

                  island->add_character(std::move(chr));
              }

          } else if (str_cmp(conf->symbol().name(), "neutral") == 0) {
              auto chr = ::skyland::alloc_entity<BasicCharacter>(
                  island, &APP.player(), coord, is_replicant);

              if (chr) {
                  chr->set_race(race);

                  chr->set_icon(icon);

                  id = chr->id();
                  island->add_character(std::move(chr));
              }
          }

          return lisp::make_integer(id);
      }}},
    {"click",
     {3,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, user_data);

          RoomCoord coord;
          coord.x = lisp::get_op(1)->integer().value_;
          coord.y = lisp::get_op(0)->integer().value_;

          if (auto room = ((Island*)lisp::get_op(2)->user_data().obj_)
                              ->get_room(coord)) {
              room->select(coord);
          }

          return L_NIL;
      }}},
    {"sel-move",
     {3,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          L_EXPECT_OP(1, integer);
          L_EXPECT_OP(2, user_data);

          RoomCoord& sel = globals().near_cursor_loc_;

          if (auto ws = APP.scene().cast_world_scene()) {
              if (lisp::get_op(2)->user_data().obj_ == &APP.player_island()) {
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
     {3,
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
     {3,
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
    {"island-pos",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, user_data);
          auto island = (Island*)lisp::get_op(1)->user_data().obj_;
          return L_CONS(L_INT(island->visual_origin().x.as_integer()),
                        L_INT(island->visual_origin().y.as_integer()));
      }}},
    {"island-configure",
     {2,
      [](int argc) {
          L_EXPECT_OP(0, cons);
          L_EXPECT_OP(1, user_data);

          auto island = (Island*)lisp::get_op(1)->user_data().obj_;

          configure_island(*island, lisp::get_op(0));

          return L_NIL;
      }}},
    {"flag-show",
     {2,
      [](int argc) {
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
              show_island_interior(island);
          } else {
              show_island_exterior(island);
          }

          return L_NIL;
      }}},
    {"exit",
     {0,
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
     {0,
      [](int argc) {
          if (APP.macrocosm()) {
              // return lisp::make_integer(macrocosm(*app).data_->p().coins_.get());
              return L_NIL;
          } else {
              return lisp::make_integer(APP.coins());
          }
      }}},
    {"coins-add",
     {1,
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
     {1,
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
    {"coins-victory", {0, [](int argc) { return L_INT(APP.victory_coins()); }}},
    {"eval-file",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, string);

          auto str = lisp::get_op(0)->string().value();

          return APP.invoke_script(str);
      }}},
    {"choice",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          return lisp::make_integer(rng::choice(
              lisp::get_op(0)->integer().value_, rng::critical_state));
      }}},
    {"autopilot",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, cons);

          APP.swap_player<AutopilotPlayer>(lisp::get_op(0));

          return L_NIL;
      }}},
    {"read-ini",
     {3,
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
    {"get-line-of-file",
     {2,
      [](int argc) {
          L_EXPECT_OP(1, string);
          L_EXPECT_OP(0, integer);

          auto line = get_line_from_file(lisp::get_op(1)->string().value(),
                                         lisp::get_op(0)->integer().value_);

          if (line) {
              info(line->c_str());
              return lisp::make_string(line->c_str());
          }

          return L_NIL;
      }}},
    {"configure-rooms",
     {1,
      [](int argc) {
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
      }}},
    {"cargo",
     {3,
      [](int argc) {
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
      }}},
    {"cargo-set",
     {4,
      [](int argc) {
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
      }}},
    {"qr-set",
     {4,
      [](int argc) {
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
      }}},
    {"achieve",
     {1,
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
    {"wg-current-type",
     {0,
      [](int argc) {
          const auto loc = APP.current_world_location();
          return L_INT((int)APP.world_graph().nodes_[loc].type_);
      }}},
    {"wg-nodes",
     {0,
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
     {3,
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
     {0, [](int argc) { return L_INT(APP.world_graph().storm_depth_); }}},
    {"wg-is-x-behind-frontier?",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);
          int x = L_LOAD_INT(0);
          return lisp::make_boolean(is_x_behind_storm_frontier(x, 0));
      }}},
    {"wg-storm-frontier-set",
     {1,
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
     {0,
      [](int argc) {
          const auto& node =
              APP.world_graph().nodes_[APP.current_world_location()];

          return L_CONS(L_INT(APP.zone() - 1),
                        L_CONS(L_INT(node.coord_.x), L_INT(node.coord_.y)));
      }}},
    {"gui-add-node",
     {2,
      [](int argc) {
          L_EXPECT_OP(0, string);
          L_EXPECT_OP(1, string);
          APP.scene().gui_add_node(nullptr, L_LOAD_STRING(1), L_LOAD_STRING(0));
          return L_NIL;
      }}},
    {"gui-delete-node",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, string);
          APP.scene().gui_delete_node(L_LOAD_STRING(0));
          return L_NIL;
      }}},
    {"fatal",
     {1,
      [](int argc) {
          lisp::DefaultPrinter p;
          format(lisp::get_op(0), p);
          Platform::fatal(p.data_.c_str());

          return L_NIL;
      }}},
    {"gui-set-attr",
     {3,
      [](int argc) {
          L_EXPECT_OP(1, string);
          L_EXPECT_OP(2, string);
          APP.scene().gui_set_attr(
              L_LOAD_STRING(2), L_LOAD_STRING(1), lisp::get_op0());
          return L_NIL;
      }}},
    {"construction-sites",
     {2,
      [](int argc) {
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
      }}},
    {"setvar",
     {2,
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
     {1,
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
    {"save-bit-load",
     {1,
      [](int argc) {
          L_EXPECT_OP(0, integer);

          return L_INT(APP.gp_.stateflags_.get(L_LOAD_INT(0)));
      }}},
    {"save-bit-store",
     {2,
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
    {"challenge-complete",
     {1,
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

    return {0, nullptr};
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

    lisp::init();

    msg("export api...");

    lisp::NativeInterface ni;
    ni.resolve_intern_sym_ = binding_intern_sym_resolver;
    ni.lookup_function_ = binding_lookup_function;
    ni.lookup_name_ = binding_lookup_name;
    ni.get_symbols_ = binding_name_getter;

    lisp::register_native_interface(ni);

    if (is_developer_mode()) {
        lisp::dostring("(strict true)");
    }

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
