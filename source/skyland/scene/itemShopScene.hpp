#pragma once

#include "skyland/room_metatable.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ItemShopScene : public WorldScene
{
public:
    ItemShopScene();


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    struct ShopItem
    {
        MetaclassIndex mt_;
        u16 price_;
        u16 qty_;
    };

    using ItemsBuffer = Buffer<ShopItem, 4>;
    DynamicMemory<ItemsBuffer> items_;

    Microseconds timer_ = 0;

    Vec2<u8> cursor_;

    enum class State : u8 {
        fade_in,
        animate_box,
        ready,
    } state_ = State::fade_in;
};



} // namespace skyland
