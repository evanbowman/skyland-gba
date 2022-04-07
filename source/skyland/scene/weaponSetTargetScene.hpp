#pragma once


#include "skyland/room.hpp"
#include "worldScene.hpp"



namespace skyland
{



class WeaponSetTargetScene : public ActiveWorldScene
{
public:
    WeaponSetTargetScene(const Vec2<u8>& weapon_loc,
                         bool near = true,
                         std::optional<Vec2<u8>> initial_pos = {});


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


private:
    // Store the tile coords of the room that we're setting the target for. If
    // we stored a pointer, we'd need to make all the room pointers into
    // shared/weak pointers instead of unique pointers, which we could easily
    // do, but doing so would use more memory.
    const Vec2<u8> weapon_loc_;


    void collect_targets(Platform&, App&);


    Buffer<Vec2<u8>, 32> targets_;
    int selector_ = 0;

    Microseconds describe_room_timer_ = milliseconds(400);
    std::optional<Text> room_description_;

    bool near_;
    bool resume_far_ = false;

    Room::Group group_ = Room::Group::none;


    std::optional<Vec2<u8>> initial_pos_;
};



} // namespace skyland
