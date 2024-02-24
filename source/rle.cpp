#include "rle.hpp"



namespace rle
{



Vector<u8> encode(Vector<u8>& data)
{
    Vector<u8> result;

    auto it = data.begin();

    Optional<u8> last_val;
    int count = 0;


    while (it not_eq data.end()) {
        if (last_val) {
            if (*last_val == *it and count < 255) {
                ++count;
            } else {
                result.push_back(count);
                result.push_back(*last_val);

                last_val = *it;
                count = 1;
            }
        } else {
            last_val = *it;
            count = 1;
        }
        ++it;
    }

    if (last_val) {
        result.push_back(count);
        result.push_back(*last_val);
    }

    return result;
}



Vector<u8> decode(Vector<u8>& data)
{
    Vector<u8> result;

    auto it = data.begin();

    while (it not_eq data.end()) {
        u8 count = *it;
        ++it;
        if (it == data.end()) {
            // INVALID FORMAT! TODO: return error message.
            //
            // NOTE: because run-length encoding should include at least two
            // bytes: the run length followed by the repeated byte value.
            result.clear();
            return result;
        }
        const u8 val = *it;
        while (count) {
            --count;
            result.push_back(val);
        }
        ++it;
    }

    return result;
}



} // namespace rle
