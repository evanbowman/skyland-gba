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

#include "playerP1.hpp"
#include "skyland/network.hpp"



namespace skyland::macro
{



class FreebuildTeam : public PlayerP1, public network::Listener
{
public:
    void update(Time delta) override;


    void receive(const network::packet::MacroSetBlock& p) override;


protected:
    void update_ai(Time delta) override
    {
    }


private:
    static const auto heartbeat_interval = seconds(5);
};



} // namespace skyland::macro
