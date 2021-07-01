#pragma once


#include "worldScene.hpp"



namespace skyland {



class Room;



class WeaponSetTargetScene : public ActiveWorldScene {
public:
    WeaponSetTargetScene(const Vec2<u8>& weapon_loc);


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


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
};



} // namespace skyland
