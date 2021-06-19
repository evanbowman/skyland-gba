#pragma once


#include "script/lisp.hpp"



class Platform;



namespace skyland {



class Island;



void configure_island(Platform& pfrm,
                      Island& island,
                      lisp::Value* island_desc_lat);



void configure_island_from_codestring(Platform& pfrm,
                                      Island& island,
                                      const char* lisp_data);



} // namespace skyland
