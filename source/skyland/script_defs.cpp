#include "alloc_entity.hpp"
#include "configure_island.hpp"
#include "opponent/friendlyAI.hpp"
#include "room_metatable.hpp"
#include "rooms/core.hpp"
#include "scene/scriptHookScene.hpp"
#include "script/lisp.hpp"
#include "serial.hpp"
#include "skyland.hpp"



namespace skyland {



// Given a variable name of this format, lisp user code will never be able to
// access the variable, which is by design.
const char* app_binding_name = "'(app)";



static App* interp_get_app()
{
    auto app = lisp::get_var(app_binding_name);
    if (app->type_ not_eq lisp::Value::Type::user_data) {
        return nullptr;
    }
    return (App*)app->user_data_.obj_;
}



static Platform* interp_get_pfrm()
{
    auto pfrm = lisp::get_var("*pfrm*");
    if (pfrm->type_ not_eq lisp::Value::Type::user_data) {
        return nullptr;
    }
    return (Platform*)pfrm->user_data_.obj_;
}



std::pair<App*, Platform*> interp_get_context()
{
    return {interp_get_app(), interp_get_pfrm()};
}



void App::init_scripts(Platform& pfrm)
{
    lisp::init(pfrm);


    lisp::set_var(app_binding_name, lisp::make_userdata(this));


    lisp::set_var("print", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, string);

                      if (auto pfrm = interp_get_pfrm()) {
                          debug(*pfrm, lisp::get_op(0)->string_.value());
                      }

                      return L_NIL;
                  }));



    lisp::set_var("player", lisp::make_function([](int argc) {
                      auto app = interp_get_app();
                      return lisp::make_userdata(&app->player_island());
                  }));


    lisp::set_var("opponent", lisp::make_function([](int argc) {
                      auto app = interp_get_app();
                      if (not app->opponent_island()) {
                          while (true)
                              ;
                      }
                      return lisp::make_userdata(&*app->opponent_island());
                  }));


    lisp::set_var("await-dialog-y/n", lisp::make_function([](int argc) {
                      auto app = interp_get_app();
                      app->dialog_expects_answer_ = true;
                      return L_NIL;
                  }));


    lisp::set_var(
        "dialog", lisp::make_function([](int argc) {
            auto app = interp_get_app();
            auto pfrm = interp_get_pfrm();

            for (int i = argc - 1; i > -1; --i) {
                if (not app->dialog_buffer()) {
                    app->dialog_buffer().emplace(
                        allocate_dynamic<DialogString>(*pfrm));
                }

                if (lisp::get_op(i)->type_ not_eq lisp::Value::Type::string) {
                    if (lisp::get_op((i)) == L_NIL) {
                        return lisp::get_op((i));
                    } else {
                        return lisp::make_error(
                            lisp::Error::Code::invalid_argument_type);
                    }
                }

                **app->dialog_buffer() += lisp::get_op(i)->string_.value();
            }

            return L_NIL;
        }));


    lisp::set_var("rooms", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, user_data);

                      auto pfrm = interp_get_pfrm();

                      auto island = (Island*)lisp::get_op(0)->user_data_.obj_;
                      auto result = serialize(*pfrm, *island);
                      lisp::read(result->c_str());
                      auto ret = lisp::get_op(0);
                      lisp::pop_op();
                      return ret;
                  }));


    lisp::set_var("chrs", lisp::make_function([](int argc) {
        L_EXPECT_ARGC(argc, 1);
        L_EXPECT_OP(0, user_data);

        auto island = (Island*)lisp::get_op(0)->user_data_.obj_;

        lisp::Value* ret = lisp::get_nil();

        for (auto& room : island->rooms()) {
            for (auto& chr : room->characters()) {
                if (chr->owner() == &island->owner()) {
                    lisp::push_op(ret);
                    {
                        auto cell = lisp::make_cons(L_NIL, L_NIL);
                        lisp::push_op(cell);
                        cell->cons_.set_car(lisp::make_integer(chr->grid_position().x));
                        cell->cons_.set_cdr(lisp::make_integer(chr->grid_position().y));
                        ret = lisp::make_cons(cell, ret);
                        lisp::pop_op(); // cell
                    }
                    lisp::pop_op(); // ret
                }
            }
        }

        return ret;
    }));


    lisp::set_var(
        "chr-slots", lisp::make_function([](int argc) {
            L_EXPECT_ARGC(argc, 1);
            L_EXPECT_OP(0, user_data);

            bool matrix[16][16];

            auto island = (Island*)lisp::get_op(0)->user_data_.obj_;

            island->plot_walkable_zones(matrix);

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
                                cell->cons_.set_car(lisp::make_integer(x));
                                cell->cons_.set_cdr(lisp::make_integer(y));
                                ret = lisp::make_cons(cell, ret);
                                lisp::pop_op(); // cell
                            }
                            lisp::pop_op(); // ret
                        }
                    }
                }
            }

            return ret;
        }));


    lisp::set_var("swap-opponent", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, symbol);

                      auto [app, pfrm] = interp_get_context();

                      auto conf = lisp::get_op(0);
                      if (str_cmp(conf->symbol_.name_, "hostile") == 0) {
                          app->swap_opponent<EnemyAI>();
                      } else if (str_cmp(conf->symbol_.name_, "neutral") == 0) {
                          app->swap_opponent<FriendlyAI>();
                      } else {
                          StringBuffer<30> err("bad ai sym: '");
                          err += conf->symbol_.name_;
                          pfrm->fatal(err.c_str());
                      }

                      return L_NIL;
                  }));


    lisp::set_var("init-opponent", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 2);
                      L_EXPECT_OP(1, integer);
                      L_EXPECT_OP(0, symbol);

                      auto [app, pfrm] = interp_get_context();

                      auto conf = lisp::get_op(0);
                      if (str_cmp(conf->symbol_.name_, "hostile") == 0) {
                          app->swap_opponent<EnemyAI>();
                      } else if (str_cmp(conf->symbol_.name_, "neutral") == 0) {
                          app->swap_opponent<FriendlyAI>();
                      } else {
                          StringBuffer<30> err("bad ai sym: '");
                          err += conf->symbol_.name_;
                          pfrm->fatal(err.c_str());
                      }

                      app->opponent_island().emplace(
                          *pfrm,
                          Layer::map_1_ext,
                          lisp::get_op(1)->integer_.value_,
                          app->opponent());

                      return L_NIL;
                  }));


    lisp::set_var("terrain", lisp::make_function([](int argc) {
        if (argc == 2) {
            L_EXPECT_OP(0, integer);
            L_EXPECT_OP(1, user_data);

            auto island = (Island*)lisp::get_op(1)->user_data_.obj_;
            island->init_terrain(*interp_get_pfrm(),
                                 lisp::get_op(0)->integer_.value_);

        } else if (argc == 1) {
            L_EXPECT_OP(0, user_data);

            auto island = (Island*)lisp::get_op(0)->user_data_.obj_;
            return lisp::make_integer(island->terrain().size());
        }
        return L_NIL;
    }));


    lisp::set_var(
        "add-room", lisp::make_function([](int argc) {
            L_EXPECT_ARGC(argc, 2);
            L_EXPECT_OP(0, cons);
            L_EXPECT_OP(1, user_data);

            auto [app, pfrm] = interp_get_context();

            auto island = (Island*)lisp::get_op(1)->user_data_.obj_;
            auto name = lisp::get_list(lisp::get_op(0), 0)->symbol_.name_;
            u8 x = lisp::get_list(lisp::get_op(0), 1)->integer_.value_;
            u8 y = lisp::get_list(lisp::get_op(0), 2)->integer_.value_;

            if (auto c = load_metaclass(name)) {
                (*c)->create(*pfrm, island, Vec2<u8>{x, y});
            } else {
                info(*pfrm, name);
                while (true)
                    ;
            }

            return L_NIL;
        }));


    lisp::set_var("rem-chr", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 3);
                      L_EXPECT_OP(0, integer); // y
                      L_EXPECT_OP(1, integer); // x
                      L_EXPECT_OP(2, user_data);

                      auto island = (Island*)lisp::get_op(2)->user_data_.obj_;

                      auto coord = Vec2<u8>{
                          (u8)lisp::get_op(1)->integer_.value_,
                          (u8)lisp::get_op(0)->integer_.value_,
                      };

                      island->remove_character(coord);

                      return L_NIL;
                  }));


    lisp::set_var("add-chr", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 3);
                      L_EXPECT_OP(0, integer); // y
                      L_EXPECT_OP(1, integer); // x
                      L_EXPECT_OP(2, user_data);

                      auto island = (Island*)lisp::get_op(2)->user_data_.obj_;

                      auto app = interp_get_app();

                      auto coord = Vec2<u8>{
                          (u8)lisp::get_op(1)->integer_.value_,
                          (u8)lisp::get_op(0)->integer_.value_,
                      };

                      auto chr = alloc_entity<BasicCharacter>(
                          island, &app->player(), coord);

                      island->add_character(std::move(chr));

                      return L_NIL;
                  }));


    lisp::set_var("add-hostile-chr", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 3);
                      L_EXPECT_OP(0, integer); // y
                      L_EXPECT_OP(1, integer); // x
                      L_EXPECT_OP(2, user_data);

                      auto app = interp_get_app();

                      auto island = (Island*)lisp::get_op(2)->user_data_.obj_;

                      auto coord = Vec2<u8>{
                          (u8)lisp::get_op(1)->integer_.value_,
                          (u8)lisp::get_op(0)->integer_.value_,
                      };

                      auto chr = alloc_entity<BasicCharacter>(
                          island, &app->opponent(), coord);

                      island->add_character(std::move(chr));

                      return L_NIL;
                  }));



    lisp::set_var("configure-player", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 2);
                      L_EXPECT_OP(0, cons);
                      L_EXPECT_OP(1, user_data);

                      auto [app, pfrm] = interp_get_context();
                      auto island = (Island*)lisp::get_op(1)->user_data_.obj_;

                      configure_island(*pfrm, *island, lisp::get_op(0));

                      return L_NIL;
                  }));


    lisp::set_var("show-flag", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, user_data);

                      auto island = (Island*)lisp::get_op(0)->user_data_.obj_;
                      island->show_flag(true);

                      return L_NIL;
                  }));


    lisp::set_var("zone", lisp::make_function([](int argc) {
                      return lisp::make_integer(interp_get_app()->zone() - 1);
                  }));


    lisp::set_var("exit-level", lisp::make_function([](int argc) {
                      interp_get_app()->exit_level() = true;
                      return L_NIL;
                  }));


    lisp::set_var("add-coins", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, integer);

                      auto app = interp_get_app();
                      app->coins() += lisp::get_op(0)->integer_.value_;

                      app->coins() = std::max(0, app->coins());

                      return L_NIL;
                  }));


    lisp::set_var(
        "eval-other-file", lisp::make_function([](int argc) {
            L_EXPECT_ARGC(argc, 1);
            L_EXPECT_OP(0, string);

            if (auto pfrm = interp_get_pfrm()) {
                auto str = lisp::get_op(0)->string_.value();
                if (auto contents = pfrm->load_file_contents("scripts", str)) {
                    lisp::dostring(contents, [pfrm](lisp::Value& v) {
                        pfrm->fatal(lisp::Error::get_string(v.error_.code_));
                    });
                } else {
                    StringBuffer<32> err("script '");
                    err += str;
                    err += "' missing";
                    pfrm->fatal(err.c_str());
                }
            }
            return L_NIL;
        }));


    lisp::set_var("cr-choice", lisp::make_function([](int argc) {
                      L_EXPECT_ARGC(argc, 1);
                      L_EXPECT_OP(0, integer);
                      return lisp::make_integer(
                          rng::choice(lisp::get_op(0)->integer_.value_,
                                      rng::critical_state));
                  }));
}



} // namespace skyland
