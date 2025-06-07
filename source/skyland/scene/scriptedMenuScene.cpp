////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "scriptedMenuScene.hpp"
#include "script/listBuilder.hpp"
#include "scriptHookScene.hpp"
#include "skyland/script_defs.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScriptedMenuScene::ScriptedMenuScene(const char* script_name)
    : menu_name_(script_name)
{
}



static void set_attr(lisp::Value* lat, const char* key, lisp::Value* v)
{
    bool found = false;

    lisp::l_foreach(lat->cons().cdr(), [&](lisp::Value* val) {
        if (str_eq(val->cons().car()->symbol().name(), key)) {
            val->cons().set_cdr(v);
            found = true;
        }
    });

    if (not found) {
        auto new_mapping = L_CONS(L_SYM(key), v);
        lat->cons().set_cdr(L_CONS(new_mapping, lat->cons().cdr()));
    }
}



static lisp::Value* get_attr(lisp::Value* lat, const char* key)
{
    lisp::Value* ret = nullptr;

    lisp::l_foreach(lat->cons().cdr(), [&](lisp::Value* val) {
        if (ret) {
            return;
        }
        if (str_eq(val->cons().car()->symbol().name(), key)) {
            ret = val->cons().cdr();
        }
    });

    if (not ret) {
        Platform::fatal(format("missing attr %", key));
    }
    return ret;
}



static int get_int_attr(lisp::Value* lat, const char* key)
{
    return get_attr(lat, key)->integer().value_;
}



static const char* get_str_attr(lisp::Value* lat, const char* key)
{
    return get_attr(lat, key)->string().value();
}



static lisp::Value* get_elem_by_id(lisp::Value* root, const char* id)
{
    lisp::Value* elem = nullptr;
    l_foreach(root, [&](lisp::Value* v) {
        if (elem) {
            return;
        }
        if (str_eq(get_str_attr(v, "id"), id)) {
            elem = v;
            return;
        }
    });

    if (not elem) {
        Platform::fatal(format("id lookup failed for %", id));
    }

    return elem;
}



void ScriptedMenuScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    StringBuffer<96> path;
    path = "/scripts/data/gui/";
    path += menu_name_;
    path += ".menu.lisp";

    Vector<char> file;
    if (APP.load_file(path.c_str(), file)) {
        lisp::VectorCharSequence seq(file);
        lisp::read(seq);
        lisp::Protected reader_result = lisp::get_op0();
        lisp::pop_op();
        lisp::eval(reader_result);
        model_ = lisp::get_op0();
        lisp::pop_op();

        lisp::l_foreach(*model_, [&](lisp::Value* v) {
            auto front = v->cons().car()->symbol().name();
            if (str_eq(front, "code")) {
                auto src = get_str_attr(v, "src");
                APP.invoke_script(src);
            }
        });
    }

    repaint_model();
}



void ScriptedMenuScene::repaint_model()
{
    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }

    if (model_) {
        using lisp::l_foreach;
        l_foreach(*model_, [&](lisp::Value* v) {
            auto front = v->cons().car()->symbol().name();
            if (str_eq(front, "text")) {
                u8 x = get_int_attr(v, "x");
                u8 y = get_int_attr(v, "y");
                auto str = get_str_attr(v, "val");
                Text::print(str, {x, y});
            } else if (str_eq(front, "rect")) {
                u8 x = get_int_attr(v, "x");
                u8 y = get_int_attr(v, "y");
                u8 w = get_int_attr(v, "w");
                u8 h = get_int_attr(v, "h");
                u8 t = get_int_attr(v, "tile");
                for (int i = x; i < x + w; ++i) {
                    for (int j = y; j < y + h; ++j) {
                        PLATFORM.set_tile(Layer::overlay, i, j, t);
                    }
                }
            } else if (str_eq(front, "md-icon")) {
                u8 x = get_int_attr(v, "x");
                u8 y = get_int_attr(v, "y");
                int icon = get_int_attr(v, "icon");
                int mem = get_int_attr(v, "mem");
                draw_image(mem, x, y, 4, 4, Layer::overlay);
                PLATFORM.load_overlay_chunk(mem, icon, 16);
            } else if (str_eq(front, "row")) {
                u8 x = get_int_attr(v, "x");
                u8 y = get_int_attr(v, "y");
                int t = get_int_attr(v, "t");
                u8 w = get_int_attr(v, "w");
                u8 p = get_int_attr(v, "p");

                for (int i = x; i < x + w; i += p) {
                    PLATFORM.set_tile(Layer::overlay, i, y, t);
                }
            } else if (str_eq(front, "col")) {
                u8 x = get_int_attr(v, "x");
                u8 y = get_int_attr(v, "y");
                int t = get_int_attr(v, "t");
                u8 h = get_int_attr(v, "w");
                u8 p = get_int_attr(v, "p");

                for (int i = y; i < y + h; i += p) {
                    PLATFORM.set_tile(Layer::overlay, x, i, t);
                }
            }
        });
    }
}



void ScriptedMenuScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    invoke_hook("on-menu-exit");
    PLATFORM.fill_overlay(0);
}



ScenePtr ScriptedMenuScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (auto next = process_script_menu_request()) {
        return next;
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::left)) {
        invoke_hook("on-L");
    }

    if (test_key(Key::right)) {
        invoke_hook("on-R");
    }

    if (test_key(Key::up)) {
        invoke_hook("on-U");
    }

    if (test_key(Key::down)) {
        invoke_hook("on-D");
    }

    if (test_key(Key::action_1)) {
        invoke_hook("on-A");
    }

    if (test_key(Key::action_2)) {
        invoke_hook("on-B");
    }

    if (needs_repaint_) {
        repaint_model();
    }

    return null_scene();
}



void ScriptedMenuScene::display()
{
    ActiveWorldScene::display();
}



void ScriptedMenuScene::gui_add_node(const char* parent_id,
                                     const char* id,
                                     const char* type)
{
    if (not model_) {
        return;
    }

    lisp::Protected attr = L_CONS(L_SYM("id"), lisp::make_string(id));
    lisp::ListBuilder elem;
    elem.push_back(L_SYM(type));
    elem.push_back(attr);
    model_ = L_CONS(elem.result(), *model_);

    needs_repaint_ = true;
}



void ScriptedMenuScene::gui_delete_node(const char* id)
{
    // if (auto n = xml::find_by_attr(model_.root(), "id", id)) {
    //     n->dead_ = true;
    //     needs_repaint_ = true;
    // }
}



void ScriptedMenuScene::gui_set_attr(const char* id,
                                     const char* attr,
                                     lisp::Value* v)
{
    if (not model_) {
        return;
    }
    set_attr(get_elem_by_id(*model_, id), attr, v);
    needs_repaint_ = true;
}



} // namespace skyland
