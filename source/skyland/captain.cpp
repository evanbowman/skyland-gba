////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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


#include "captain.hpp"
#include "skyland/island.hpp"
#include "systemString.hpp"



namespace skyland
{



static CaptainAbility current_ability = CaptainAbility::none;



void bind_captain_ability(CaptainAbility ability)
{
    current_ability = ability;
}



CaptainAbility current_captain_ability()
{
    return current_ability;
}



bool ability_active(CaptainAbility cap)
{
    return current_captain_ability() == cap;
}



u16 captain_icon(CaptainAbility cap)
{
    switch (cap) {
    case CaptainAbility::range:
        return 20;

    case CaptainAbility::rate:
        return 21;

    case CaptainAbility::terrain:
        return 22;

    case CaptainAbility::warp:
        return 23;

    case CaptainAbility::none:
        break;
    };

    return 0;
}



StringBuffer<64> captain_name(CaptainAbility cap)
{
    StringBuffer<64> result;

    int index = (int)cap;
    if (index >= (int)CaptainAbility::none) {
        return result;
    }

    auto& pfrm = Platform::instance();
    auto names = SYSTR(captain_names);

    auto ptr = names->c_str();

    while (index) {
        while (*ptr not_eq '#') {
            ++ptr;
        }
        ++ptr;
        --index;
    }

    while (*ptr not_eq '#') {
        result.push_back(*(ptr++));
    }

    return result;
}



StringBuffer<500> captain_desc(CaptainAbility cap)
{
    StringBuffer<500> result;

    int index = (int)cap;
    if (index >= (int)CaptainAbility::none) {
        return result;
    }

    auto& pfrm = Platform::instance();
    auto names = SYSTR(captain_descs);

    auto ptr = names->c_str();

    while (index) {
        while (*ptr not_eq '#') {
            ++ptr;
        }
        ++ptr;
        --index;
    }

    while (*ptr not_eq '#') {
        result.push_back(*(ptr++));
    }

    return result;
}



void rebind_captain(App& app)
{
    auto try_bind_captain = [&app](auto& chr) {
        if (chr->owner() not_eq &player(app)) {
            return false;
        }
        auto icon = chr->get_icon();
        for (int i = 0; i < (int)CaptainAbility::none; ++i) {
            if (captain_icon((CaptainAbility)i) == icon) {
                bind_captain_ability((CaptainAbility)i);
                return true;
            }
        }

        return false;
    };

    for (auto& room : player_island(app).rooms()) {
        for (auto& chr : room->characters()) {
            if (try_bind_captain(chr)) {
                return;
            }
        }
    }
    for (auto& room : opponent_island(app)->rooms()) {
        for (auto& chr : room->characters()) {
            if (try_bind_captain(chr)) {
                return;
            }
        }
    }
    bind_captain_ability(CaptainAbility::none);
}



} // namespace skyland
