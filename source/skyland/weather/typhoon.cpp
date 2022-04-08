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
                       return custom_color(0x270d42);
                   case 2:
                       return custom_color(0x444c63);
                   case 3:
                       return custom_color(0x95bbbd);
                   case 5:
                       return custom_color(0xde2c4a);
                   case 7:
                       return custom_color(0x106469);
                   case 8:
                       return custom_color(0x90de81);
                   case 9:
                       return custom_color(0xd9dbdb);
                   case 10:
                       if (not player_island(app).interior_visible()) {
                           return custom_color(0x80acb0);
                       }
                       break;
                   case 12:
                       return custom_color(0x115ba6);
                   case 13:
                       return custom_color(0xdee7a5);
                   case 14:
                       return custom_color(0xaab87d);
                   case 15:
                       return custom_color(0x475c32);
                   }
                   break;

               case ShaderPalette::tile1:
                   switch (index) {
                   case 1:
                       return custom_color(0x270d42);
                   case 2:
                       return custom_color(0x444c63);
                   case 3:
                       return custom_color(0x95bbbd);
                   case 4:
                       if (opponent_island(app) and not
                           opponent_island(app)->interior_visible()) {
                           return custom_color(0x322f59);
                       }
                       break;
                   case 5:
                       return custom_color(0xde2c4a);
                   case 7:
                       return custom_color(0x106469);
                   case 8:
                       return custom_color(0x90de81);
                   case 9:
                       return custom_color(0xd9dbdb);
                   case 10: // FIXME
                       if (opponent_island(app) and
                           not opponent_island(app)->interior_visible()) {
                           return custom_color(0x838c6b);
                       }
                       break;
                   case 12:
                       return custom_color(0x115ba6);
                   case 13:
                       return custom_color(0xdee7a5);
                   case 14:
                       return custom_color(0xaab87d);
                   case 15:
                       return custom_color(0x475c32);
                   }
                   break;

               case ShaderPalette::background:
                   switch (index) {
                   case 1:
                       return custom_color(0x5d818f);
                   case 2:
                       return custom_color(0xcdd1d0);
                   case 3:
                       return custom_color(0x80acb0);
                   case 4:
                       return custom_color(0x395e70);
                   }
                   break;

               case ShaderPalette::spritesheet:
                   switch (index) {
                   case 2:
                       return custom_color(0x492f5e);

                   case 8:
                       return custom_color(0x444c63);

                   case 11:
                       return custom_color(0x95bbbd);

                   case 12:
                       return custom_color(0x270d42);

                   default:
                       break;
                   }
                   break;

               default:
                   return k;
               }

               return k;
           };
}




}
