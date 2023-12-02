#include <stddef.h>
#include <string.h>



// NOTE: for GBA, the arm compiler drags in junk code from newlib. And I really
// mean junk. No offense to the people who wrote the newlib libc implementation,
// but it's really not a good fit for gbadev. The functions below aren't
// particularly optimized for gba, but speed isn't my gripe with newlib. The
// library does all sorts of stuff for reentrancy, which isn't applicable to the
// gba console and dumps several kilobytes of junk into iwram. Whenever I see a
// function from libc in the game's linker map file, I copy-paste an
// implementation of the function here, to protect my code against newlib's
// stupid decisions.



void abort()
{
    // I mean, why would you even call abort on an embedded system?!
    while (1)
        ;
}



// Well, of course we don't ever want to exit. The game stops when the player
// flips the power switch.
int __aeabi_atexit(void* _, void (*__)(void*), void* ___)
{
    return 0;
}



size_t strlen(const char* str)
{
    register const char *s;
    for (s = str; *s; ++s);
    return(s - str);
}



char* strchr(const char* p, int ch)
{
    char c;

    c = ch;

    for (;; ++p) {
        if (*p == c) {
            return ((char *)p);
        }
        if (*p == '\0') {
            return NULL;
        }
    }
}



int memcmp(const void* s1, const void* s2, size_t n)
{
    if (n != 0) {
        register const unsigned char *p1 = s1, *p2 = s2;
        do {
            if (*p1++ != *p2++)
                return (*--p1 - *--p2);
        } while (--n != 0);
    }
    return (0);
}



int abs (int i)
{
    return i < 0 ? -i : i;
}



#define isdigit(c) (c >= '0' && c <= '9')
/*
 * Copyright (C) 2014, Galois, Inc.
 * This sotware is distributed under a standard, three-clause BSD license.
 * Please see the file LICENSE, distributed with this software, for specific
 * terms and conditions.
 */
double atof(const char *s)
{
    // This function stolen from either Rolf Neugebauer or Andrew Tolmach.
    // Probably Rolf.
    double a = 0.0;
    int e = 0;
    int c;
    while ((c = *s++) != '\0' && isdigit(c)) {
        a = a*10.0 + (c - '0');
    }
    if (c == '.') {
        while ((c = *s++) != '\0' && isdigit(c)) {
            a = a*10.0 + (c - '0');
            e = e-1;
        }
    }
    if (c == 'e' || c == 'E') {
        int sign = 1;
        int i = 0;
        c = *s++;
        if (c == '+')
            c = *s++;
        else if (c == '-') {
            c = *s++;
            sign = -1;
        }
        while (isdigit(c)) {
            i = i*10 + (c - '0');
            c = *s++;
        }
        e += i*sign;
    }
    while (e > 0) {
        a *= 10.0;
        e--;
    }
    while (e < 0) {
        a *= 0.1;
        e++;
    }
    return a;
}
