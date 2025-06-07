////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////



extern const unsigned char script_init[];
//;
extern const unsigned char script_pre_levelgen[];
//;
extern const unsigned char script_post_levelgen[];
//

static const struct
{
    const char* name_;
    const unsigned char* data_;
} scripts[] = {

    {"init.lisp", script_init},
    //;
    {"pre_levelgen.lisp", script_pre_levelgen},
    //;
    {"post_levelgen.lisp", script_post_levelgen},
    //
};
