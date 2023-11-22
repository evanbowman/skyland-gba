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


#include "scriptHookScene.hpp"
#include "script/lisp.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



ScenePtr<Scene> ScriptHookScene::update(App& app, Microseconds delta)
{
    invoke_hook(app, invoke_hook_name_.c_str());

    return next_scene_();
}



void invoke_hook(App& app,
                 const char* lisp_hook_name,
                 const InvokeHookConfig& config)
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
            app.time_stream().clear();
            time_stream::event::Initial e;
            app.time_stream().push(app.level_timer(), e);
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
