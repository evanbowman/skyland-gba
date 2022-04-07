#include "string.hpp"



// In most cases, you do not want to call this function directly, better to call
// the localized version, locale_num2str. Only call english__to_string for
// logging purposes, where the language is assumed to be english.
void english__to_string(int num, char* buffer, int base);


template <u32 length> StringBuffer<length> to_string(int num)
{
    char temp[length];
    english__to_string(num, temp, 10);

    return temp;
}



StringBuffer<12> stringify(s32 num)
{
    return to_string<12>(num);
}
