#pragma once

#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class ConstructionScene : public ActiveWorldScene
{
public:
    ConstructionScene(Platform& pfrm, bool near = true)
        : data_(allocate_dynamic<Data>(pfrm, "construction-data")), near_(near)
    {
    }


    ConstructionScene(Platform& pfrm, int selector)
        : selector_(selector),
          data_(allocate_dynamic<Data>(pfrm, "construction-data"))
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


    u32 selector_ = 0;

    std::optional<Text> text_;
    std::optional<Text> category_label_;

    struct Data
    {
        Buffer<Coord, 48> construction_sites_;
        Buffer<MetaclassIndex, 100> available_buildings_;
        std::optional<MetaclassIndex> last_constructed_building_;
    };

    DynamicMemory<Data> data_;

    bool show_category_ = false;
    Room::Category last_category_ = Room::Category::count;

    State state_ = State::select_loc;

    int building_selector_ = 0;

    int touchscroll_ = 0;
    int last_touch_x_ = 0;

    bool near_;
};



} // namespace skyland
