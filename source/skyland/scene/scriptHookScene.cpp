#include "scriptHookScene.hpp"
#include "script/lisp.hpp"



namespace skyland {



ScenePtr<Scene> ScriptHookScene::update(Platform& pfrm,
                                        App& app,
                                        Microseconds delta)
{
    auto fn = lisp::get_var(invoke_hook_name_.c_str());

    if (fn->type_ == lisp::Value::Type::function) {
        lisp::funcall(fn, 0);

        auto result = lisp::get_op(0);
        if (result->type_ == lisp::Value::Type::error) {
            StringBuffer<32> err("err invoking ");
            err += invoke_hook_name_;
            pfrm.fatal(err.c_str());
        }

        lisp::pop_op(); // funcall result

    } else {
        StringBuffer<32> err("hook ");
        err += invoke_hook_name_;
        err += " is not lambda";
        pfrm.fatal(err.c_str());
    }

    return next_scene_();
}




}
