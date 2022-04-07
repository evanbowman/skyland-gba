#include "environment.hpp"
#include "skyland/island.hpp"



namespace skyland::weather
{



class Storm : public CleanEnvironment
{
    Platform::Screen::Shader shader(App& app) const override
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
};




}
