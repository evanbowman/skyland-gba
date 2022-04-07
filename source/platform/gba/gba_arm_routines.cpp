////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
// All of the code in this file will be compiled as arm code, and placed in the
// IWRAM section of the executable. The system has limited memory for IWRAM
// calls, so limit this file to performace critical code, or code that must be
// defined in IWRAM.
//
////////////////////////////////////////////////////////////////////////////////


#ifdef __GBA__
#define IWRAM_CODE __attribute__((section(".iwram"), long_call))
#else
#define IWRAM_CODE
#endif // __GBA__


#include "gba.h"
#include "mixer.hpp"


s16 parallax_table[280];
s16 vertical_parallax_table[280];



#define REG_SGFIFOA *(volatile u32*)0x40000A0
#define REG_SGFIFOB *(volatile u32*)0x40000A4
