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
#include "eternal/eternal.hpp"
#include "macrocosmEngine.hpp"
#include "platform/flash_filesystem.hpp"
#include "player/autopilotPlayer.hpp"
#include "player/opponent/enemyAI.hpp"
#include "player/opponent/friendlyAI.hpp"
#include "player/opponent/procgenEnemyAI.hpp"
#include "room_metatable.hpp"
#include "rooms/cargoBay.hpp"
#include "rooms/core.hpp"
#include "scene/constructionScene.hpp"
#include "scene/scriptHookScene.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "serial.hpp"
#include "sharedVariable.hpp"
#include "skyland.hpp"
#include "skyland/entity/projectile/arcBolt.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/entity/projectile/ionBurst.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/entity/projectile/nemesisBlast.hpp"
#include "skyland/entity/projectile/pluginProjectile.hpp"
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
                  save::store_global_data(*lisp::interp_get_pfrm(), app->gp_);
              }

              return L_INT(prev);
          }},
         {"log",
          [](int argc) {
              L_EXPECT_ARGC(argc, 1);

              if (auto pfrm = lisp::interp_get_pfrm()) {
                  if (lisp::get_op(0)->type() == lisp::Value::Type::string) {
                      debug(*pfrm, lisp::get_op(0)->string().value());
                  } else {
                      lisp::DefaultPrinter p;
                      format(lisp::get_op(0), p);
                      debug(*pfrm, p.data_.c_str());
                  }
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

                  save::store_global_data(*lisp::interp_get_pfrm(),
                                          interp_get_app()->gp_);
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
         {"instance-count",
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
         {"startup-time",
          [](int argc) {
              lisp::ListBuilder builder;
              DateTime dt;
              if (lisp::interp_get_pfrm()->system_call("startup-time", &dt)) {
                  builder.push_back(lisp::make_integer(dt.date_.year_));
                  builder.push_back(lisp::make_integer(dt.date_.month_));
                  builder.push_back(lisp::make_integer(dt.date_.day_));
                  builder.push_back(lisp::make_integer(dt.hour_));
                  builder.push_back(lisp::make_integer(dt.minute_));
                  builder.push_back(lisp::make_integer(dt.second_));
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

              Text t(*lisp::interp_get_pfrm(), {L_LOAD_U8(1), L_LOAD_U8(0)});

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
              GenericPool::print_diagnostics(*lisp::interp_get_pfrm());
              return L_NIL;
          }},
         {"synth-notes-store",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto pfrm = lisp::interp_get_pfrm();

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              synth_notes_store(*pfrm, *island, L_LOAD_STRING(0));

              return L_NIL;
          }},
         {"synth-notes-load",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto pfrm = lisp::interp_get_pfrm();

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              synth_notes_load(*pfrm, *island, L_LOAD_STRING(0));

              return L_NIL;
          }},
         {"speaker-data-store",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto pfrm = lisp::interp_get_pfrm();

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              speaker_data_store(*pfrm, *island, L_LOAD_STRING(0));

              return L_NIL;
          }},
         {"speaker-data-load",
          [](int argc) {
              L_EXPECT_ARGC(argc, 2);
              L_EXPECT_OP(1, user_data);
              L_EXPECT_OP(0, string);

              auto pfrm = lisp::interp_get_pfrm();

              auto island = (Island*)lisp::get_op(1)->user_data().obj_;

              speaker_data_load(*pfrm, *island, L_LOAD_STRING(0));

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



DynamicMemory<FileLine>
get_line_from_file(Platform& pfrm, const char* file_name, int line)
{
    --line; // From the caller's perspective, file lines start from 1.

    auto result = allocate_dynamic<FileLine>("file-line");

    if (!result) {
        return result;
    }

    if (auto contents = pfrm.load_file_contents("", file_name)) {

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
    {"dialog-await-y/n",
     [](int argc) {
         auto app = interp_get_app();
         state_bit_store(*app, StateBit::dialog_expects_answer, true);
         return L_NIL;
     }},
    {"repl",
     [](int argc) {
         auto app = interp_get_app();
         state_bit_store(*app, StateBit::launch_repl, true);
         return L_NIL;
     }},
    {"diff",
     [](int argc) {
         auto app = interp_get_app();
         return L_INT((int)app->persistent_data().difficulty_);
     }},
    {"diff-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);
         auto app = interp_get_app();
         app->persistent_data().difficulty_ =
             (PersistentData::Difficulty)L_LOAD_INT(0);
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

         x %= 8;
         y %= 8;
         z %= 9;

         macrocosm(*interp_get_app()).sector().set_block({x, y, z}, type);

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
             if (m.data_->origin_sector_.name() == str) {
                 m.bind_sector(m.data_->origin_sector_.coordinate());
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

         for (auto& sector : m.data_->outpost_sectors_) {
             lisp::Protected x(L_INT(sector.coordinate().x));
             lisp::Protected y(L_INT(sector.coordinate().y));
             result.push_back(L_CONS(x, y));
         }

         lisp::Protected x(L_INT(m.data_->origin_sector_.coordinate().x));
         lisp::Protected y(L_INT(m.data_->origin_sector_.coordinate().y));
         result.push_back(L_CONS(x, y));

         return result.result();
     }},
    {"key-bind",
     [](int argc) {
         L_EXPECT_ARGC(argc, 2);
         L_EXPECT_OP(1, string);
         L_EXPECT_OP(0, symbol);

         auto s = lisp::get_op(0)->symbol();

         KeyCallbackProcessor::Binding b{KeyCallbackProcessor::MatchSeq{},
                                         [s](Platform& pfrm, App& app) {
                                             // Bad hack: construct dummy symbol.
                                             auto fn = lisp::get_var(s.name());
                                             lisp::funcall(fn, 0);
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
         L_EXPECT_OP(0, integer); // y
         L_EXPECT_OP(1, integer); // x
         L_EXPECT_OP(2, user_data);

         auto [app, pfrm] = interp_get_context();

         auto island = (Island*)lisp::get_op(0)->user_data().obj_;

         const u8 x = L_LOAD_INT(1);
         const u8 y = L_LOAD_INT(0);

         island->fire_create(*pfrm, *app, {x, y});

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
                     lisp::ListBuilder chr_info;
                     chr_info.push_back(
                         lisp::make_integer(chr->grid_position().x));
                     chr_info.push_back(
                         lisp::make_integer(chr->grid_position().y));
                     if (chr->health() not_eq 255) {
                         chr_info.push_back(lisp::make_integer(chr->health()));
                     }
                     if (chr->is_replicant()) {
                         chr_info.push_back(lisp::make_integer(1));
                     }
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

         island->plot_walkable_zones(*interp_get_app(), matrix);

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
    {"port",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         switch (L_LOAD_INT(0)) {
         case 1:
             lisp::interp_get_pfrm()->remote_console().start();
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
         app->swap_opponent<ProcgenEnemyAI>(rng::critical_state, 1);
         if (auto p = dynamic_cast<ProcgenEnemyAI*>(&app->opponent())) {
             p->set_levelgen_count(lisp::get_op(0)->integer().value_);
             p->generate_level(*pfrm, *app);
         }

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

         app->create_opponent_island(*pfrm, lisp::get_op(1)->integer().value_);

         return L_NIL;
     }},
    {"terrain",
     [](int argc) {
         if (argc == 2) {
             L_EXPECT_OP(0, integer);
             L_EXPECT_OP(1, user_data);

             auto island = (Island*)lisp::get_op(1)->user_data().obj_;
             island->init_terrain(*lisp::interp_get_pfrm(),
                                  lisp::get_op(0)->integer().value_);

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
             (*c)->create(*pfrm, *app, island, RoomCoord{x, y});
             island->repaint(*pfrm, *app);
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

         island->destroy_room(*pfrm, *app, coord);

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
    {"chr-hp",
     [](int argc) {
         L_EXPECT_ARGC(argc, 4);
         L_EXPECT_OP(1, integer); // y
         L_EXPECT_OP(2, integer); // x
         L_EXPECT_OP(3, user_data);

         auto island = (Island*)lisp::get_op(3)->user_data().obj_;

         auto coord = RoomCoord{
             (u8)lisp::get_op(2)->integer().value_,
             (u8)lisp::get_op(1)->integer().value_,
         };

         auto arg0 = lisp::get_op(0);

         if (auto chr = island->character_at_location(coord)) {
             if (arg0->type() == lisp::Value::Type::integer) {
                 chr->__set_health(arg0->integer().value_);
             }
             return lisp::make_integer(chr->health());
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

                     auto path = find_path(*lisp::interp_get_pfrm(),
                                           *interp_get_app(),
                                           island,
                                           {startx, starty},
                                           {destx, desty});

                     if (path and *path) {
                         chr->set_movement_path(*lisp::interp_get_pfrm(),
                                                *interp_get_app(),
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
         L_EXPECT_OP(0, integer);
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

         const bool is_replicant = lisp::get_op(0)->integer().value_;

         auto conf = lisp::get_op(1);
         if (str_cmp(conf->symbol().name(), "hostile") == 0) {
             app->swap_opponent<EnemyAI>();
             auto chr = ::skyland::alloc_entity<BasicCharacter>(
                 island, &app->opponent(), coord, is_replicant);

             if (chr) {
                 island->add_character(std::move(chr));
             }
         } else if (str_cmp(conf->symbol().name(), "neutral") == 0) {
             auto chr = ::skyland::alloc_entity<BasicCharacter>(
                 island, &app->player(), coord, is_replicant);

             if (chr) {
                 island->add_character(std::move(chr));
             }
         }

         return L_NIL;
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
                 room->select(*lisp::interp_get_pfrm(), *app, coord);
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
             RoomCoord& sel =
                 std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

             if (auto ws = dynamic_cast<WorldScene*>(&app->scene())) {
                 if (lisp::get_op(2)->user_data().obj_ ==
                     &app->player_island()) {
                     sel =
                         std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
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

         configure_island(*pfrm, *app, *island, lisp::get_op(0));

         return L_NIL;
     }},
    {"flag-show",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, user_data);

         auto island = (Island*)lisp::get_op(0)->user_data().obj_;
         island->show_flag(true);

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
             return lisp::make_integer(macrocosm(*app).data_->p().coins_.get());
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
             auto current = macrocosm(*app).data_->p().coins_.get();
             current += L_LOAD_INT(0);
             macrocosm(*app).data_->p().coins_.set(
                 std::min(std::numeric_limits<macro::Coins>::max(), current));
         } else {
             app->set_coins(*lisp::interp_get_pfrm(),
                            std::max(0, (int)(L_LOAD_INT(0) + app->coins())));
         }

         return L_NIL;
     }},
    {"coins-set",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, integer);

         auto app = interp_get_app();
         if (app->macrocosm()) {
             auto val = L_LOAD_INT(0);
             macrocosm(*app).data_->p().coins_.set(
                 std::min(std::numeric_limits<macro::Coins>::max(), val));
         } else {
             app->set_coins(*lisp::interp_get_pfrm(), L_LOAD_INT(0));
         }


         return L_NIL;
     }},
    {"coins-victory",
     [](int argc) { return L_INT(interp_get_app()->victory_coins()); }},
    {"eval-file",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, string);

         if (auto pfrm = lisp::interp_get_pfrm()) {

             auto app = interp_get_app();
             if (app == nullptr) {
                 while (true)
                     ;
                 return L_NIL;
             }

             auto str = lisp::get_op(0)->string().value();

             return app->invoke_script(*pfrm, str);
         } else {
             while (true)
                 ;
         }
         return L_NIL;
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

         auto line = get_line_from_file(*lisp::interp_get_pfrm(),
                                        lisp::get_op(1)->string().value(),
                                        lisp::get_op(0)->integer().value_);

         if (line) {
             info(*lisp::interp_get_pfrm(), line->c_str());
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
             if (auto cb = dynamic_cast<CargoBay*>(room)) {
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
             if (auto cb = dynamic_cast<CargoBay*>(room)) {
                 cb->set_cargo(lisp::get_op(0)->string().value(), 1);
             }
         }

         return L_NIL;
     }},
    {"register-sprite",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, string);

         img::Image texture;

         Vector<char> data;

         const char* path = lisp::get_op(0)->string().value();

         flash_filesystem::read_file_data_text(
             *lisp::interp_get_pfrm(), path, data);

         if (data.size() >= sizeof texture) {
             auto it = data.begin();
             for (u32 i = 0; i < sizeof texture; ++i) {
                 ((u8*)&texture)[i] = *it;
                 ++it;
             }

             auto result =
                 interp_get_app()->custom_sprite_mapper().map_image(texture);
             return lisp::make_integer(SpriteTile::custom_sprite_tile_begin +
                                       result);
         } else {
             StringBuffer<64> err = "invalid sprite path or contents: ";
             err += path;
             Platform::fatal(err.c_str());
         }

         return L_NIL;
     }},
    {"register-tile",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, string);

         img::Image texture;

         Vector<char> data;

         const char* path = lisp::get_op(0)->string().value();

         flash_filesystem::read_file_data_text(
             *lisp::interp_get_pfrm(), path, data);

         if (data.size() >= sizeof texture) {
             auto it = data.begin();
             for (u32 i = 0; i < sizeof texture; ++i) {
                 ((u8*)&texture)[i] = *it;
                 ++it;
             }

             auto result =
                 interp_get_app()->custom_tile_mapper().map_image(texture);

             return lisp::make_integer(Tile::dlc_tiles_begin + result);
         } else {
             StringBuffer<64> err = "invalid texture path or contents: ";
             err += path;
             Platform::fatal(err.c_str());
         }

         return L_NIL;
     }},
    {"register-room",
     [](int argc) {
         L_EXPECT_ARGC(argc, 1);
         L_EXPECT_OP(0, cons);

         if (not plugin_room_register(lisp::get_op(0))) {
             info(*lisp::interp_get_pfrm(), "failed to register plugin room");
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

         achievements::raise(
             *lisp::interp_get_pfrm(), *interp_get_app(), achievement);

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
    {"emit", [](int argc) {
         L_EXPECT_ARGC(argc, 6);
         L_EXPECT_OP(4, user_data);
         L_EXPECT_OP(3, integer);
         L_EXPECT_OP(2, integer);
         L_EXPECT_OP(1, integer);
         L_EXPECT_OP(0, integer);

         auto& app = *interp_get_app();
         auto& pfrm = *lisp::interp_get_pfrm();

         auto island = (Island*)lisp::get_op(4)->user_data().obj_;

         const u8 x1 = lisp::get_op(3)->integer().value_;
         const u8 y1 = lisp::get_op(2)->integer().value_;

         auto room = island->get_room({x1, y1});
         if (not room) {
             Platform::fatal(format("cannot emit from (%,%), "
                                    "which is not a room!",
                                    x1,
                                    y1)
                                 .c_str());
         }

         auto start = room->center();

         const u8 x2 = lisp::get_op(1)->integer().value_;
         const u8 y2 = lisp::get_op(0)->integer().value_;

         auto other_island = room->other_island(app);
         if (not other_island) {
             return L_NIL;
         }

         auto target = room->other_island(app)->origin();
         target.x += x2 * 16 + 8;
         target.y += y2 * 16 + 8;

         if (lisp::get_op(5)->type() == lisp::Value::Type::cons) {
             auto tile = lisp::get_list(lisp::get_op(5), 0);
             if (tile->type() not_eq lisp::Value::Type::integer) {
                 Platform::fatal("Projectile param list expected type "
                                 "integer in slot zero");
             }
             auto damage = lisp::get_list(lisp::get_op(5), 1);
             if (tile->type() not_eq lisp::Value::Type::integer) {
                 Platform::fatal("Projectile param list expected type "
                                 "integer in slot one");
             }
             auto flip = lisp::get_list(lisp::get_op(5), 2);
             if (tile->type() not_eq lisp::Value::Type::integer) {
                 Platform::fatal("Projectile param list expected tyep "
                                 "integer in slot one");
             }

             app.camera()->shake(4);

             cannon_sound.play(pfrm, 3);

             auto c = app.alloc_entity<PluginProjectile>(
                 pfrm,
                 start,
                 target,
                 room->parent(),
                 room->position(),
                 (u16)tile->integer().value_,
                 (Health)damage->integer().value_,
                 (bool)flip->integer().value_);
             if (c) {
                 room->parent()->projectiles().push(std::move(c));
             }
             return L_NIL;
         } else if (lisp::get_op(5)->type() not_eq lisp::Value::Type::symbol) {
             Platform::fatal("Invalid argument for (emit): "
                             "first arg should be a symbol or "
                             "a list.");
         }

         auto name = lisp::get_op(5)->symbol().name();

#define MAKE_PROJECTILE(NAME, CLASS, SOUND)                                    \
    if (str_eq(name, NAME)) {                                                  \
        auto c = app.alloc_entity<CLASS>(                                      \
            pfrm, start, target, room->parent(), room->position());            \
        if (c) {                                                               \
            SOUND.play(pfrm, 3);                                               \
            room->parent()->projectiles().push(std::move(c));                  \
        }                                                                      \
    }

         app.camera()->shake(4);

         // clang-format off

                 MAKE_PROJECTILE("cannonball", Cannonball, cannon_sound)
                 else MAKE_PROJECTILE("arcbolt", ArcBolt, cannon_sound)
                 else MAKE_PROJECTILE("decimator-burst", DecimatorBurst, cannon_sound)
                 else MAKE_PROJECTILE("flak", Flak, cannon_sound)
                 else MAKE_PROJECTILE("ion-burst", IonBurst, cannon_sound)
                 else MAKE_PROJECTILE("nemesis-blast", NemesisBlast, cannon_sound)
                 else if (str_eq(name, "missile")) {
                     missile_sound.play(pfrm, 3);
                     auto c = app.alloc_entity<Missile>(pfrm,
                                                        start,
                                                        target,
                                                        room->position().x,
                                                        room->position().y,
                                                        room->parent());
                     if (c) {
                         room->parent()->projectiles().push(std::move(c));
                     }
                 } else {
                     Platform::fatal(
                         format("cannot make projectile of type %", name)
                             .c_str());
                 }

    // clang-format on

#undef MAKE_PROJECTILE

         return L_NIL;
     }}};



void App::init_scripts(Platform& pfrm, Function<16, void(const char*)> msg)
{
    msg("lisp init...");

    lisp::init(pfrm);

    msg("export api...");

    lisp::bind_functions(script_api,
                         sizeof(script_api) / sizeof(script_api[0]));

    __app = this;

    // NOTE: we need to disable custom scripts during startup, otherwise,
    // someone could irreversibly mess up a game.
    const bool was_developer_mode = is_developer_mode();
    set_developer_mode(false);

    msg("import lisp stdlib...");

    auto str = pfrm.load_file_contents("scripts", "init.lisp");
    if (str) {
        lisp::BasicCharSequence seq(str);
        lisp::dostring(seq, [&pfrm](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            pfrm.fatal(p.data_.c_str());
        });
    }

    set_developer_mode(was_developer_mode);
}



} // namespace skyland
