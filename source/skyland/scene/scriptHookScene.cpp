#include "scriptHookScene.hpp"
#include "script/lisp.hpp"



namespace skyland {



ScenePtr<Scene>
ScriptHookScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    invoke_hook(pfrm, invoke_hook_name_.c_str());

    return next_scene_();
}



void invoke_hook(Platform& pfrm, const char* lisp_hook_name)
{
    auto fn = lisp::get_var(lisp_hook_name);

    if (fn->type_ == lisp::Value::Type::function) {
        lisp::funcall(fn, 0);

        auto result = lisp::get_op(0);
        if (result->type_ == lisp::Value::Type::error) {
            StringBuffer<32> err("err invoking ");
            err += lisp_hook_name;
            pfrm.fatal(err.c_str());
        }

        lisp::pop_op(); // funcall result

    } else if (fn->type_ not_eq lisp::Value::Type::nil) {
        StringBuffer<32> err("hook ");
        err += lisp_hook_name;
        err += " is not lambda";
        pfrm.fatal(err.c_str());
    }
}



} // namespace skyland
