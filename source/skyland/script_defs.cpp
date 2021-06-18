#include "skyland.hpp"
#include "script/lisp.hpp"
#include "room_metatable.hpp"
#include "rooms/core.hpp"
#include "configure_island.hpp"



namespace skyland {



static App* interp_get_app()
{
    auto game = lisp::get_var("*app*");
    if (game->type_ not_eq lisp::Value::Type::user_data) {
        return nullptr;
    }
    return (App*)game->user_data_.obj_;
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


    lisp::set_var("*app*", lisp::make_userdata(this));


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
            while (true) ;
        }
        return lisp::make_userdata(&*app->opponent_island());
    }));


    lisp::set_var("init-opponent", lisp::make_function([](int argc) {
        L_EXPECT_ARGC(argc, 1);
        L_EXPECT_OP(0, integer);

        auto [app, pfrm] = interp_get_context();

        app->opponent_island().emplace(*pfrm,
                                          Layer::map_1_ext,
                                          lisp::get_op(0)->integer_.value_,
                                          app->opponent());

        return L_NIL;
    }));


    lisp::set_var("add-room", lisp::make_function([](int argc) {
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
            while (true) ;
        }

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

        auto island = (Island*)lisp::get_op(1)->user_data_.obj_;
        island->show_flag(true);

        return L_NIL;
    }));


    lisp::set_var("missile-ammo", lisp::make_function([](int argc) {
        L_EXPECT_ARGC(argc, 2);
        L_EXPECT_OP(0, integer);
        L_EXPECT_OP(1, user_data);

        auto island = (Island*)lisp::get_op(1)->user_data_.obj_;
        island->owner().missile_ammo() = lisp::get_op(0)->integer_.value_;

        return L_NIL;
    }));


    lisp::set_var("eval-other-file", lisp::make_function([](int argc) {
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



}
