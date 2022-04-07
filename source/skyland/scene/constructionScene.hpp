#pragma once

#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland
{



static const int construction_zone_min_y = 4;



class ConstructionScene : public ActiveWorldScene
{
public:
    ConstructionScene(bool near = true)
        : data_(allocate_dynamic<Data>("construction-data")), near_(near)
    {
    }


    ConstructionScene(int selector)
        : selector_(selector),
          data_(allocate_dynamic<Data>("construction-data"))
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    bool camera_update_check_key(Platform& pfrm, App& app) override;


private:
    enum class State {
        select_loc,
        choose_building,
        add_terrain,
        insufficient_funds,
    };


    Island* island(App& app);


    bool collect_available_buildings(Platform&, App&);


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

    bool show_category_ = true;
    Room::Category last_category_ = Room::Category::count;

    State state_ = State::select_loc;

    int building_selector_ = 0;

    int touchscroll_ = 0;
    int last_touch_x_ = 0;

    bool near_;
};



} // namespace skyland
