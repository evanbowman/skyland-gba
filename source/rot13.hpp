#pragma once



inline char rot13(char ascii_char)
{
    if (ascii_char >= 'A' && ascii_char <= 'Z') {
        ascii_char = 'A' + (ascii_char - 'A' + 13) % 26;
    } else if (ascii_char >= 'a' && ascii_char <= 'z') {
        ascii_char = 'a' + (ascii_char - 'a' + 13) % 26;
    }
    return ascii_char;
}
