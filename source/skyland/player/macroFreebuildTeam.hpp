////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
    void update(App& app, Microseconds delta) override;


    void receive(App&, const network::packet::MacroSetBlock& p) override;


private:
    static const auto heartbeat_interval = seconds(5);
    Microseconds heartbeat_send_counter_ = 0;
    Microseconds heartbeat_recv_counter_ = 0;
};



} // namespace skyland::macro
