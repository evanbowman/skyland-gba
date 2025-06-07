////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "entity.hpp"
#include "globals.hpp"



namespace skyland
{



GlobalEntityListData::Pool& GlobalEntityListData::pool() const
{
    return globals().entity_node_pool_;
}



} // namespace skyland
