#pragma once

#include <stddef.h>



extern "C" {



void abort();



char* gcvt(double number, int ndigit, char* buf);



size_t strlen(const char* str);
char* strchr(const char* p, int ch);



int memcmp(const void* s1, const void* s2, size_t n);
void* memset(void* s, int c, size_t n);
void* memcpy(void* dest, const void* src, size_t n);



int abs(int i);



double atof(const char* s);
}
