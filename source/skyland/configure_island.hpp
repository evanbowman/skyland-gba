////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "script/value.hpp"



class Platform;



namespace skyland
{



class App;
class Island;



void configure_island(Island& island, lisp::Value* island_desc_lat);



void configure_island_from_codestring(Island& island, const char* lisp_data);



} // namespace skyland
