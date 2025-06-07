////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////



#if defined(__NDS__)
#define EXT_WORKRAM_DATA
#elif defined(__GBA__)
#define EXT_WORKRAM_DATA __attribute__((section(".ewram")))
#else
#define EXT_WORKRAM_DATA
#endif
