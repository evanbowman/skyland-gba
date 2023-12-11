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


#include "scriptHookScene.hpp"
#include "script/lisp.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



ScenePtr<Scene> ScriptHookScene::update(Time delta)
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
