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



namespace skyland
{



struct LoginToken
{
    static const int size = 8;

    char text_[size] = {'b', 'a', 'd', 'i', 'n', 'i', 't', 'p'};
    bool valid_ = false;
};



extern LoginToken __login_token;



} // namespace skyland
