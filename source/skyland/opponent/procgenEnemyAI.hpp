#pragma once


#include "enemyAI.hpp"



namespace skyland {




class ProcgenEnemyAI : public EnemyAI {
public:


    void update(Platform& pfrm, App& app, Microseconds delta) override;


private:

    void generate_level(Platform& pfrm, App& app);

};




}
