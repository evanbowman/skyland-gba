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


#include "typhoon.hpp"



namespace skyland::weather
{



void Typhoon::update(Platform& pfrm, App& app, Microseconds delta)
{
    // ...
}



void Typhoon::display(Platform& pfrm, App& app)
{

}



const char* Typhoon::music() const
{
    return "solecism";
}



Platform::Screen::Shader Typhoon::shader(App& app) const
{
    return [&app](ShaderPalette palette, ColorConstant k, int arg, int index) {
               switch (palette) {
               case ShaderPalette::tile0:
                   switch (index & 0x0f) {
                   case 1:
                       return custom_color(0x10405c);
                   case 2:
                       return custom_color(0x5e728c);
                   case 3:
                       return custom_color(0x95bbbd);
                   case 5:
                       return custom_color(0xc7612e);
                   case 8:
                       return custom_color(0xb8ea80);
                   case 9:
                       return custom_color(0xe8ebe6);
                   case 10:
                       if (not player_island(app).interior_visible()) {
                           return custom_color(0x9adbd6);
                       }
                       break;
                   case 12:
                       return custom_color(0x1477b5);
                   case 13:
                       return custom_color(0xdee7a5);
                   case 14:
                       return custom_color(0xaab87d);
                   case 15:
                       return custom_color(0x5f6e3b);
                   }
                   break;

               case ShaderPalette::tile1:
                   switch (index) {
                   case 1:
                       return custom_color(0x10405c);
                   case 2:
                       return custom_color(0x5e728c);
                   case 3:
                       return custom_color(0x95bbbd);
                   case 5:
                       return custom_color(0xc7612e);
                   case 8:
                       return custom_color(0xb8ea80);
                   case 9:
                       return custom_color(0xe8ebe6);
                   case 10: // FIXME
                       if (opponent_island(app) and
                           not opponent_island(app)->interior_visible()) {
                           return custom_color(0x899668);
                       }
                       break;
                   case 12:
                       return custom_color(0x1477b5);
                   case 13:
                       return custom_color(0xdee7a5);
                   case 14:
                       return custom_color(0xaab87d);
                   case 15:
                       return custom_color(0x5f6e3b);
                   }
                   break;

               case ShaderPalette::background:
                   switch (index) {
                   case 1:
                       return custom_color(0x6fbdb9);
                   case 2:
                       return custom_color(0xe8ebe6);
                   case 3:
                       return custom_color(0x9adbd6);
                   case 4:
                       return custom_color(0x47b1ba);
                   }
                   break;

               default:
                   return k;
               }

               return k;
           };
}




}
