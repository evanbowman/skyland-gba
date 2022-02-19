#include "systemString.hpp"



namespace skyland {



SystemStringBuffer loadstr(Platform& pfrm, SystemString str)
{
    auto result = allocate_dynamic<StringBuffer<1900>>(pfrm);

    const char* file = "english.txt";

    if (auto data = pfrm.load_file_contents("strings", file)) {
        const int target_line = static_cast<int>(str);

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
        Platform::fatal(format("missing language file %", file).c_str());
    }
}



}
