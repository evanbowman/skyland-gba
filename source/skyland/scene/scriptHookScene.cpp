////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "scriptHookScene.hpp"
#include "script/lisp.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



ScenePtr ScriptHookScene::update(Time delta)
{
    invoke_hook(invoke_hook_name_.c_str());

    return next_scene_();
}



void invoke_hook(const char* lisp_hook_name, const InvokeHookConfig& config)
{
    auto var_name_sym = lisp::make_symbol(lisp_hook_name);

    auto fn = lisp::get_nil();

    if (var_name_sym->type() == lisp::Value::Type::symbol) {
        fn = lisp::get_var(var_name_sym);
    }

    if (fn->type() == lisp::Value::Type::function) {

        lisp::safecall(fn, 0);

        // Not worth trying to roll back lisp code, do not allow rewind if we've
        // run a script.
        if (config.reset_timestream_) {
            APP.time_stream().clear();
            time_stream::event::Initial e;
            APP.time_stream().push(APP.level_timer(), e);
        }

        lisp::pop_op(); // funcall result

    } else if (fn->type() not_eq lisp::Value::Type::nil) {
        StringBuffer<32> err("hook ");
        err += lisp_hook_name;
        err += " is not lambda";
        PLATFORM.fatal(err.c_str());
    }
}



} // namespace skyland
