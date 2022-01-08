#pragma once

#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class ConstructionScene : public ActiveWorldScene {
public:
    ConstructionScene(bool near = true) :
        near_(near)
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
        insufficient_funds,
    };


    Island* island(App& app);


    void collect_available_buildings(Platform&, App&);


    void find_construction_sites(Platform&, App&);


    void msg(Platform& pfrm, const char* text);


    void show_current_building_text(Platform& pfrm, App& app);


    using Coord = Vec2<u8>;

    Buffer<Coord, 12> construction_sites_;
    u32 selector_ = 0;

    std::optional<Text> text_;

    State state_ = State::select_loc;

    int building_selector_ = 0;

    Buffer<RoomMeta*, 20> available_buildings_;
    const RoomMeta* last_constructed_building_ = nullptr;

    bool near_;
};


} // namespace skyland
