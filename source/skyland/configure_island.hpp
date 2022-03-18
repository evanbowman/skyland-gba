#pragma once


#include "script/lisp.hpp"



class Platform;



namespace skyland
{



class App;
class Island;



void configure_island(Platform& pfrm,
                      App& app,
                      Island& island,
                      lisp::Value* island_desc_lat);



void configure_island_from_codestring(Platform& pfrm,
                                      App& app,
                                      Island& island,
                                      const char* lisp_data);



} // namespace skyland
