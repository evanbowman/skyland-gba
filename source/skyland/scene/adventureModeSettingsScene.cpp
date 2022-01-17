#include "adventureModeSettingsScene.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland {



void AdventureModeSettingsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    const char* difficulty_str = "difficulty:";

    difficulty_text_.emplace(
        pfrm,
        difficulty_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(difficulty_str)),
                     1});


    const char* easy_str = "easy";

    easy_text_.emplace(
        pfrm,
        easy_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(easy_str)), 4});


    const char* normal_str = "normal";

    normal_text_.emplace(
        pfrm,
        normal_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(normal_str)), 6});


    const char* hard_str = "hard";

    hard_text_.emplace(
        pfrm,
        hard_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(hard_str)), 8});


    app.persistent_data().difficulty_ = PersistentData::Difficulty::experienced;

    pfrm.screen().fade(0.96f);
    pfrm.screen().fade(1.f);
}



void AdventureModeSettingsScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    difficulty_text_.reset();
    easy_text_.reset();
    normal_text_.reset();
    hard_text_.reset();
}



ScenePtr<Scene>
AdventureModeSettingsScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::action_1)) {
        switch (app.persistent_data().difficulty_) {
        case PersistentData::Difficulty::beginner:
            app.invoke_script(pfrm, "/scripts/config/easy/score.lisp");
            break;

        case PersistentData::Difficulty::experienced:
            app.invoke_script(pfrm, "/scripts/config/normal/score.lisp");
            break;

        case PersistentData::Difficulty::expert:
            app.invoke_script(pfrm, "/scripts/config/hard/score.lisp");
            break;
        }

        return scene_pool::alloc<WorldMapScene>();
    }



    return null_scene();
}



} // namespace skyland
