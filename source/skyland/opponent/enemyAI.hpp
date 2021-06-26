#pragma once


#include "opponent.hpp"
#include "skyland/coins.hpp"



namespace skyland {



class Cannon;
class MissileSilo;
class BasicCharacter;



class EnemyAI : public Opponent {
public:
    void update(Platform&, App&, Microseconds delta) override;


    void on_room_damaged(Platform&, App& app, Room&) override;


    void add_coins(Coins value)
    {
        coins_ += value;
    }


private:
    void set_target(Platform&, App&, const u8 matrix[16][16], Cannon& cannon);
    void
    set_target(Platform&, App&, const u8 matrix[16][16], MissileSilo& silo);


    void assign_boarded_character(Platform&, App&, BasicCharacter& character);


    void assign_local_character(Platform&, App&, BasicCharacter& character);


    static const auto next_action_timeout = seconds(1);

    Microseconds next_action_timer_ = next_action_timeout;

    static const auto character_reassign_timeout = seconds(6);

    Microseconds character_reassign_timer_ = character_reassign_timeout;

    Coins coins_ = 0;
};



} // namespace skyland
