#pragma once

#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class ConstructionScene : public ActiveWorldScene {
public:
    ConstructionScene(bool near = true) : near_(near)
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

    // NOTE: while the terrain isn't quite so large, due to holes in a player's
    // island, we may need more construction sites than there are x-coordinates.
    // TODO: increase the size of the construction_sites_ buffer.
    Buffer<Coord, 18> construction_sites_;
    u32 selector_ = 0;

    std::optional<Text> text_;

    State state_ = State::select_loc;

    int building_selector_ = 0;

    // NOTE: A metaclass pointer would be easier to work with, but metaclass
    // indices require only two bytes, so we can make the buffer twice as large.
    Buffer<MetaclassIndex, 40> available_buildings_;
    std::optional<MetaclassIndex> last_constructed_building_;

    bool near_;
};



} // namespace skyland
