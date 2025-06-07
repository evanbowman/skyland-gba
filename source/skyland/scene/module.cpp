////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "module.hpp"
#include "string.hpp"



namespace skyland
{



detail::_Module::Factory* detail::_Module::factories_;
detail::_Module::Factory* detail::_Module::developer_mode_factories_;


} // namespace skyland
