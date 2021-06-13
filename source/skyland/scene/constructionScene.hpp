#pragma once

#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class ConstructionScene : public WorldScene {
public:
    ConstructionScene()
    {
    }


    ConstructionScene(int selector) : selector_(selector)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


private:
    enum class State {
        select_loc,
        choose_building,
        add_terrain,
        insufficent_funds,
    };


    void collect_available_buildings(Platform&, App&);


    void find_construction_sites(Platform&, App&);


    void msg(Platform& pfrm, const char* text);


    void show_current_building_text(Platform& pfrm);


    using Coord = Vec2<u8>;

    Buffer<Coord, 12> construction_sites_;
    u32 selector_ = 0;

    std::optional<Text> text_;

    State state_ = State::select_loc;

    int building_selector_ = 0;

    Buffer<RoomMeta*, 20> available_buildings_;
};


} // namespace skyland
