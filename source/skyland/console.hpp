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

#include "boxed.hpp"
#include "platform/platform.hpp"



namespace skyland
{



class App;



class ConsoleState
{
public:
    class Impl
    {
    public:
        virtual ~Impl()
        {
        }


        using Self = Boxed<Impl, Impl, 1900>;

        virtual void on_text(Self&, Platform::RemoteConsole::Line& line);
    };


    Impl::Self impl_;
};



} // namespace skyland
