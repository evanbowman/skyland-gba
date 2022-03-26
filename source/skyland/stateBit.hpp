#pragma once

#include "bitvector.hpp"



namespace skyland
{



enum class StateBit {
    dialog_expects_answer,
    launch_repl,
    surrender_offered,
    remote_console_force_newline,
    count,
};



class App;



void state_bit_store(App& app, StateBit state_bit, bool value);



bool state_bit_load(App& app, StateBit state_bit);



using StateBitvector = Bitvector<(int)StateBit::count>;



} // namespace skyland
