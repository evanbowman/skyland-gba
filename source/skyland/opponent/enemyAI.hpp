#pragma once


#include "opponent.hpp"
#include "skyland/coins.hpp"



namespace skyland {



class Drone;
class Cannon;
class FlakGun;
class DroneBay;
class IonCannon;
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
    void
    set_target(Platform&, App&, const u8 matrix[16][16], Room& generic_gun);
    void
    set_target(Platform&, App&, const u8 matrix[16][16], MissileSilo& silo);

    void
    set_target(Platform&, App&, const u8 matrix[16][16], IonCannon& cannon);


    void set_target(Platform&, App&, const u8 matrix[16][16], FlakGun& gun);


    void
    combat_drone_set_target(Platform&, App&, u8 matrix[16][16], Drone& drone);


    void offensive_drone_set_target(Platform&,
                                    App&,
                                    const u8 matrix[16][16],
                                    Drone& drone);


    void update_room(Platform&, App&, const u8 matrix[16][16], DroneBay& db);


    void assign_boarded_character(Platform&, App&, BasicCharacter& character);


    void assign_local_character(Platform&, App&, BasicCharacter& character);


    void resolve_insufficient_power(Platform&, App&);


    static const auto next_action_timeout = seconds(1);

    Microseconds next_action_timer_ = next_action_timeout;

    static const auto character_reassign_timeout = seconds(6);

    Microseconds character_reassign_timer_ = character_reassign_timeout;

    Coins coins_ = 0;

    Microseconds score_subtract_timer_ = 0;
    Microseconds total_time_ = 0;
};



} // namespace skyland
