#include "skyland.hpp"
#include "script/lisp.hpp"
#include "room_metatable.hpp"
#include "rooms/core.hpp"



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

        lisp::foreach(lisp::get_op(0), [&](lisp::Value* val) {
            auto name_symb = lisp::get_list(val, 0);
            if (name_symb->type_ not_eq lisp::Value::Type::symbol) {
                return;
            }
            u8 x = lisp::get_list(val, 1)->integer_.value_;
            u8 y = lisp::get_list(val, 2)->integer_.value_;

            if (auto c = load_metaclass(name_symb->symbol_.name_)) {
                (*c)->create(*pfrm, island, Vec2<u8>{x, y});
            }
        });
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



}



}
