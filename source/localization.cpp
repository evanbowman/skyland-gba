#include "localization.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"



StringBuffer<32> format_time(u32 seconds, bool include_hours)
{
    StringBuffer<32> result;
    char buffer[32];

    int hours = seconds / 3600;
    int remainder = (int)seconds - hours * 3600;
    int mins = remainder / 60;
    remainder = remainder - mins * 60;
    int secs = remainder;

    if (include_hours) {
        locale_num2str(hours, buffer, 10);
        result += buffer;
        result += ":";
    }

    locale_num2str(mins, buffer, 10);
    result += buffer;
    result += ":";

    if (secs < 10) {
        result += "0";
    }

    locale_num2str(secs, buffer, 10);
    result += buffer;

    return result;
}



class str_const
{
private:
    const char* const p_;
    const size_t sz_;

public:
    template <size_t N>
    constexpr str_const(const char (&a)[N]) : p_(a), sz_(N - 1)
    {
    }

    constexpr char operator[](std::size_t n)
    {
        return n < sz_ ? p_[n] : '0';
    }
};


#ifndef __BYTE_ORDER__
#error "byte order must be defined"
#endif


// FIXME: assumes little endian? Does it matter though, which way we order
// stuff, as long as it's consistent? Actually it does matter, considering
// that we're byte-swapping stuff in unicode.hpp
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#error "TODO: fix the utf-8 decoding (below) for big endian"
#endif


// Needs to be a macro because there's no way to pass a str_const as a
// constexpr parameter. Converts the first utf-8 codepoint in a string to a
// 32-bit integer, for use in a giant switch statement (below).
#define UTF8_GETCHR(_STR_)                                                     \
    []() -> utf8::Codepoint {                                                  \
        if constexpr ((str_const(_STR_)[0] & 0x80) == 0) {                     \
            return str_const(_STR_)[0];                                        \
        } else if constexpr ((str_const(_STR_)[0] & 0xf0) == 0xC0 ||           \
                             (str_const(_STR_)[0] & 0xf0) == 0xD0) {           \
            return (u32)(u8)str_const(_STR_)[0] |                              \
                   (((u32)(u8)str_const(_STR_)[1]) << 8);                      \
        } else if constexpr ((str_const(_STR_)[0] & 0xf0) == 0xE0) {           \
            return (u32)(u8)str_const(_STR_)[0] |                              \
                   (((u32)(u8)str_const(_STR_)[1]) << 8) |                     \
                   ((u32)(u8)str_const(_STR_)[2] << 16);                       \
        } else if constexpr ((str_const(_STR_)[0] & 0xf0) == 0xF0) {           \
            return (u32)(u8)str_const(_STR_)[0] |                              \
                   ((u32)(u8)str_const(_STR_)[1] << 8) |                       \
                   ((u32)(u8)str_const(_STR_)[2] << 16) |                      \
                   ((u32)(u8)str_const(_STR_)[3] << 24);                       \
        } else {                                                               \
            return 0;                                                          \
        }                                                                      \
    }()


template <u32 B, bool C> constexpr void my_assert()
{
    static_assert(C, "oh no");
}

#define UTF8_TESTCHR(_STR_)                                                    \
    []() -> utf8::Codepoint {                                                  \
        my_assert<(u32)(u8)str_const(_STR_)[0], false>();                      \
        return 0;                                                              \
    }()



static const char* font_image = "charset";



void set_font_image(const char* font_image_name)
{
    font_image = font_image_name;
}



std::optional<Platform::TextureMapping>
extended_charset_map(const utf8::Codepoint& cp);



std::optional<Platform::TextureMapping>
standard_charset_map(const utf8::Codepoint& cp)
{
    auto mapping = [&]() -> std::optional<u16> {
        switch (cp) {

            // clang-format off

        case UTF8_GETCHR(u8"0"): return 1;
        case UTF8_GETCHR(u8"1"): return 2;
        case UTF8_GETCHR(u8"2"): return 3;
        case UTF8_GETCHR(u8"3"): return 4;
        case UTF8_GETCHR(u8"4"): return 5;
        case UTF8_GETCHR(u8"5"): return 6;
        case UTF8_GETCHR(u8"6"): return 7;
        case UTF8_GETCHR(u8"7"): return 8;
        case UTF8_GETCHR(u8"8"): return 9;
        case UTF8_GETCHR(u8"9"): return 10;
        case UTF8_GETCHR(u8"a"): return 11;
        case UTF8_GETCHR(u8"b"): return 12;
        case UTF8_GETCHR(u8"c"): return 13;
        case UTF8_GETCHR(u8"d"): return 14;
        case UTF8_GETCHR(u8"e"): return 15;
        case UTF8_GETCHR(u8"f"): return 16;
        case UTF8_GETCHR(u8"g"): return 17;
        case UTF8_GETCHR(u8"h"): return 18;
        case UTF8_GETCHR(u8"i"): return 19;
        case UTF8_GETCHR(u8"j"): return 20;
        case UTF8_GETCHR(u8"k"): return 21;
        case UTF8_GETCHR(u8"l"): return 22;
        case UTF8_GETCHR(u8"m"): return 23;
        case UTF8_GETCHR(u8"n"): return 24;
        case UTF8_GETCHR(u8"o"): return 25;
        case UTF8_GETCHR(u8"p"): return 26;
        case UTF8_GETCHR(u8"q"): return 27;
        case UTF8_GETCHR(u8"r"): return 28;
        case UTF8_GETCHR(u8"s"): return 29;
        case UTF8_GETCHR(u8"t"): return 30;
        case UTF8_GETCHR(u8"u"): return 31;
        case UTF8_GETCHR(u8"v"): return 32;
        case UTF8_GETCHR(u8"w"): return 33;
        case UTF8_GETCHR(u8"x"): return 34;
        case UTF8_GETCHR(u8"y"): return 35;
        case UTF8_GETCHR(u8"z"): return 36;
        case UTF8_GETCHR(u8"."): return 37;
        case UTF8_GETCHR(u8","): return 38;
        case UTF8_GETCHR(u8"A"): return 39;
        case UTF8_GETCHR(u8"B"): return 40;
        case UTF8_GETCHR(u8"C"): return 41;
        case UTF8_GETCHR(u8"D"): return 42;
        case UTF8_GETCHR(u8"E"): return 43;
        case UTF8_GETCHR(u8"F"): return 44;
        case UTF8_GETCHR(u8"G"): return 45;
        case UTF8_GETCHR(u8"H"): return 46;
        case UTF8_GETCHR(u8"I"): return 47;
        case UTF8_GETCHR(u8"J"): return 48;
        case UTF8_GETCHR(u8"K"): return 49;
        case UTF8_GETCHR(u8"L"): return 50;
        case UTF8_GETCHR(u8"M"): return 51;
        case UTF8_GETCHR(u8"N"): return 52;
        case UTF8_GETCHR(u8"O"): return 53;
        case UTF8_GETCHR(u8"P"): return 54;
        case UTF8_GETCHR(u8"Q"): return 55;
        case UTF8_GETCHR(u8"R"): return 56;
        case UTF8_GETCHR(u8"S"): return 57;
        case UTF8_GETCHR(u8"T"): return 58;
        case UTF8_GETCHR(u8"U"): return 59;
        case UTF8_GETCHR(u8"V"): return 60;
        case UTF8_GETCHR(u8"W"): return 61;
        case UTF8_GETCHR(u8"X"): return 62;
        case UTF8_GETCHR(u8"Y"): return 63;
        case UTF8_GETCHR(u8"Z"): return 64;
        case UTF8_GETCHR(u8"\""): return 65;
        case UTF8_GETCHR(u8"'"): return 66;
        case UTF8_GETCHR(u8"["): return 67;
        case UTF8_GETCHR(u8"]"): return 68;
        case UTF8_GETCHR(u8"("): return 69;
        case UTF8_GETCHR(u8")"): return 70;
        case UTF8_GETCHR(u8":"): return 71;
        case UTF8_GETCHR(u8" "): return 72;
        case UTF8_GETCHR(u8"%"): return 93;
        case UTF8_GETCHR(u8"!"): return 94;
        case UTF8_GETCHR(u8"?"): return 95;
        case UTF8_GETCHR(u8"+"): return 98;
        case UTF8_GETCHR(u8"-"): return 99;
        case UTF8_GETCHR(u8"/"): return 100;
        case UTF8_GETCHR(u8"\\"): return 2156;
        case UTF8_GETCHR(u8"*"): return 101;
        case UTF8_GETCHR(u8"="): return 102;
        case UTF8_GETCHR(u8"<"): return 103;
        case UTF8_GETCHR(u8">"): return 104;
        case UTF8_GETCHR(u8"#"): return 105;
        case UTF8_GETCHR(u8"_"): return 186;
        case UTF8_GETCHR(u8"$"): return 2151;
        case UTF8_GETCHR(u8";"): return 2152;
        case UTF8_GETCHR(u8"\n"): return 2153;
        case UTF8_GETCHR(u8"`"): return 2154;
        case UTF8_GETCHR(u8"@"): return 2155;
        case UTF8_GETCHR(u8"©"): return 185;

        // Cyrillic Characters
        case UTF8_GETCHR(u8"А"): return 187;
        case UTF8_GETCHR(u8"Б"): return 188;
        case UTF8_GETCHR(u8"В"): return 189;
        case UTF8_GETCHR(u8"Г"): return 190;
        case UTF8_GETCHR(u8"Д"): return 191;
        case UTF8_GETCHR(u8"Е"): return 192;
        case UTF8_GETCHR(u8"Ж"): return 193;
        case UTF8_GETCHR(u8"З"): return 194;
        case UTF8_GETCHR(u8"И"): return 195;
        case UTF8_GETCHR(u8"Й"): return 196;
        case UTF8_GETCHR(u8"К"): return 197;
        case UTF8_GETCHR(u8"Л"): return 198;
        case UTF8_GETCHR(u8"М"): return 199;
        case UTF8_GETCHR(u8"Н"): return 200;
        case UTF8_GETCHR(u8"О"): return 201;
        case UTF8_GETCHR(u8"П"): return 202;
        case UTF8_GETCHR(u8"Р"): return 203;
        case UTF8_GETCHR(u8"С"): return 204;
        case UTF8_GETCHR(u8"Т"): return 205;
        case UTF8_GETCHR(u8"У"): return 206;
        case UTF8_GETCHR(u8"Ф"): return 207;
        case UTF8_GETCHR(u8"Х"): return 208;
        case UTF8_GETCHR(u8"Ц"): return 209;
        case UTF8_GETCHR(u8"Ч"): return 210;
        case UTF8_GETCHR(u8"Ш"): return 211;
        case UTF8_GETCHR(u8"Щ"): return 212;
        case UTF8_GETCHR(u8"Ъ"): return 213;
        case UTF8_GETCHR(u8"Ы"): return 214;
        case UTF8_GETCHR(u8"Ь"): return 215;
        case UTF8_GETCHR(u8"Э"): return 216;
        case UTF8_GETCHR(u8"Ю"): return 217;
        case UTF8_GETCHR(u8"Я"): return 218;
        case UTF8_GETCHR(u8"а"): return 219;
        case UTF8_GETCHR(u8"б"): return 220;
        case UTF8_GETCHR(u8"в"): return 221;
        case UTF8_GETCHR(u8"г"): return 222;
        case UTF8_GETCHR(u8"д"): return 223;
        case UTF8_GETCHR(u8"е"): return 224;
        case UTF8_GETCHR(u8"ж"): return 225;
        case UTF8_GETCHR(u8"з"): return 226;
        case UTF8_GETCHR(u8"и"): return 227;
        case UTF8_GETCHR(u8"й"): return 228;
        case UTF8_GETCHR(u8"к"): return 229;
        case UTF8_GETCHR(u8"л"): return 230;
        case UTF8_GETCHR(u8"м"): return 231;
        case UTF8_GETCHR(u8"н"): return 232;
        case UTF8_GETCHR(u8"о"): return 233;
        case UTF8_GETCHR(u8"п"): return 234;
        case UTF8_GETCHR(u8"р"): return 235;
        case UTF8_GETCHR(u8"с"): return 236;
        case UTF8_GETCHR(u8"т"): return 237;
        case UTF8_GETCHR(u8"у"): return 238;
        case UTF8_GETCHR(u8"ф"): return 239;
        case UTF8_GETCHR(u8"х"): return 240;
        case UTF8_GETCHR(u8"ц"): return 241;
        case UTF8_GETCHR(u8"ч"): return 242;
        case UTF8_GETCHR(u8"ш"): return 243;
        case UTF8_GETCHR(u8"щ"): return 244;
        case UTF8_GETCHR(u8"ъ"): return 245;
        case UTF8_GETCHR(u8"ы"): return 246;
        case UTF8_GETCHR(u8"ь"): return 246;
        case UTF8_GETCHR(u8"э"): return 248;
        case UTF8_GETCHR(u8"ю"): return 249;
        case UTF8_GETCHR(u8"я"): return 250;
        case UTF8_GETCHR(u8"Ґ"): return 251;
        case UTF8_GETCHR(u8"ґ"): return 252;
        case UTF8_GETCHR(u8"Є"): return 253;
        case UTF8_GETCHR(u8"є"): return 254;
        case UTF8_GETCHR(u8"Ї"): return 255;
        case UTF8_GETCHR(u8"ї"): return 256;
        case UTF8_GETCHR(u8"ё"): return 87;


            // clang-format on

        default:
            return std::nullopt;
        }
    }();
    if (mapping) {
        return Platform::TextureMapping{font_image, *mapping};
    } else {
        return extended_charset_map(cp);
    }
}


std::optional<Platform::TextureMapping>
doublesize_texture_map(const utf8::Codepoint& cp)
{
    return {};
}


std::optional<Platform::TextureMapping> null_texture_map(const utf8::Codepoint&)
{
    return {};
}


static int language_id = 0;


Platform::TextureCpMapper locale_texture_map()
{
    return standard_charset_map;
}


Platform::TextureCpMapper locale_doublesize_texture_map()
{
    return doublesize_texture_map;
}


void locale_set_language(int language_id)
{
    ::language_id = language_id;
}


// I had to add this code during chinese translation, for places where I needed
// to use traditional chinese numbers rather than arabic numerals.
const char* locale_repr_smallnum(u8 num, std::array<char, 40>& buffer)
{
    auto languages = lisp::get_var(lisp::make_symbol("languages"));

    auto lang = lisp::get_list(languages, ::language_id);

    const char* lang_name =
        lang->expect<lisp::Cons>().car()->expect<lisp::Symbol>().name_;

    if (str_cmp(lang_name, "chinese") == 0) {
        // Yeah, this is lazy. I could write a string to
        // number-to-unicode-string algorithm for chinese, but I don't feel like
        // it right now.
        switch (num) {
        default:
        case 1:
            return "一";
        case 2:
            return "二";
        case 3:
            return "三";
        case 4:
            return "四";
        case 5:
            return "五";
        case 6:
            return "六";
        case 7:
            return "七";
        case 8:
            return "八";
        case 9:
            return "九";
        case 10:
            return "十";
        case 11:
            return "十一";
        case 12:
            return "十二";
        case 13:
            return "十三";
        case 14:
            return "十四";
        case 15:
            return "十五";
        case 16:
            return "十六";
        case 17:
            return "十七";
        case 18:
            return "十八";
        case 19:
            return "十九";
        case 20:
            return "二十";
        case 21:
            return "二十一";
        case 22:
            return "二十二";
        case 23:
            return "二十三";
        case 24:
            return "二十四";
        case 25:
            return "二十五";
        case 26:
            return "二十六";
        case 27:
            return "二十七";
        case 28:
            return "二十八";
        case 29:
            return "二十九";
        case 30:
            return "三十";
        case 31:
            return "三十一";
        case 32:
            return "三十二";
        case 33:
            return "三十三";
        case 34:
            return "三十四";
        case 35:
            return "三十五";
        case 36:
            return "三十六";
        case 37:
            return "三十七";
        case 38:
            return "三十八";
        case 39:
            return "三十九";
        case 40:
            return "四十";
        case 41:
            return "四十一";
        case 42:
            return "四十二";
        case 43:
            return "四十三";
        case 44:
            return "四十四";
        case 45:
            return "四十五";
        case 46:
            return "四十六";
        case 47:
            return "四十七";
        case 48:
            return "四十八";
        case 49:
            return "四十九";
        }
    } else {
        // Arabic numerals
        locale_num2str(num, buffer.data(), 10);
        return buffer.data();
    }
}


int locale_get_language()
{
    return ::language_id;
}


StringBuffer<31> locale_language_name(int language)
{
    auto languages = lisp::get_var(lisp::make_symbol("languages"));

    auto lang = lisp::get_list(languages, language);

    return lang->expect<lisp::Cons>().car()->expect<lisp::Symbol>().name_;
}


LocalizedText locale_localized_language_name(Platform& pfrm, int language)
{
    const auto cached_lang = ::language_id;

    ::language_id = language;

    auto ret = locale_string(pfrm, LocaleString::language_name);

    ::language_id = cached_lang;

    return ret;
}


bool locale_requires_doublesize_font()
{
    auto languages = lisp::get_var(lisp::make_symbol("languages"));

    auto lang = lisp::get_list(languages, ::language_id);

    return lang->expect<lisp::Cons>()
               .cdr()
               ->expect<lisp::Cons>()
               .car()
               ->expect<lisp::Integer>()
               .value_ == 2;
}


LocalizedText locale_string(Platform& pfrm, LocaleString ls)
{
    auto result = allocate_dynamic<LocalizedStrBuffer>(pfrm, "locale-string");

    auto languages = lisp::get_var(lisp::make_symbol("languages"));

    auto lang = lisp::get_list(languages, ::language_id);

    StringBuffer<31> fname =
        lang->expect<lisp::Cons>().car()->expect<lisp::Symbol>().name_;
    fname += ".txt";

    if (auto data = pfrm.load_file_contents("strings", fname.c_str())) {
        const int target_line = static_cast<int>(ls);

        int index = 0;
        while (index not_eq target_line) {
            while (*data not_eq '\n') {
                if (*data == '\0') {
                    pfrm.fatal("null byte in localized text");
                }
                ++data;
            }
            ++data;

            ++index;
        }

        while (*data not_eq '\0' and *data not_eq '\n') {
            result->push_back(*data);
            ++data;
        }

        return result;
    } else {
        pfrm.fatal("missing strings file for language");
    }
}


void english__to_string(int num, char* buffer, int base)
{
    int i = 0;
    bool is_negative = false;

    if (num == 0) {
        buffer[i++] = '0';
        buffer[i] = '\0';
        return;
    }

    // Based on the behavior of itoa()
    if (num < 0 && base == 10) {
        is_negative = true;
        num = -num;
    }

    while (num != 0) {
        int rem = num % base;
        buffer[i++] = (rem > 9) ? (rem - 10) + 'a' : rem + '0';
        num = num / base;
    }

    if (is_negative) {
        buffer[i++] = '-';
    }

    buffer[i] = '\0';

    str_reverse(buffer, i);

    return;
}


void locale_num2str(int num, char* buffer, int base)
{
    english__to_string(num, buffer, base);
}
