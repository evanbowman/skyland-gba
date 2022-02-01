#pragma once

#include "hull.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class BronzeHull : public Hull {
public:


    BronzeHull(Island* parent, const Vec2<u8>& position);


    using Hull::Hull;


    static const char* name()
    {
        return "bronze-hull";
    }


    static bool unlocked_by_default()
    {
        // Should probably be false, as the whole purpose of decorations is to
        // reward the player for achievements.
        // // return true;

        return false;
    }


    static Conditions::Value conditions()
    {
        return Conditions::disabled_in_tutorials;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;


};



}
