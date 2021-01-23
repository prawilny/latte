#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define string char *
#define i64 long
#define BUF_SIZE 256

_Static_assert(sizeof(i64) == 8, "");

static char buf[BUF_SIZE];

void error() {
    fprintf(stderr, "runtime error\n");
    exit(1);
}

void printInt(const i64 i) {
    printf("%ld\n", i);
}

void printString(const string s) {
    printf("%s\n", s);
}

i64 readInt() {
    fgets(buf, BUF_SIZE - 1, stdin);
    return strtol(buf, NULL, 0);
}

string readString() {
    fgets(buf, BUF_SIZE, stdin);
    size_t len = strlen(buf);
    if(buf[len - 1] == '\n') {
        buf[len - 1] = '\0';
        len -= 1;
    }

    string read = (string) malloc(len + 1);
    memcpy(read, buf, len + 1);
    return read;
}

string __strcat(const string s1, const string s2) {
    size_t s1_len = strlen(s1);
    size_t s2_len = strlen(s2);

    size_t str_len = s1_len + s2_len + 1;
    string str = (string) malloc(str_len);

    memcpy(str, s1, s1_len);
    memcpy(str + s1_len, s2, s2_len + 1);

    return str;
}

void * __new(const i64 bytes, const void * vtable) {
    void * ptr = malloc(bytes);
    memset(ptr, 0, bytes);
    *((i64 *) ptr) = (i64) vtable;
    return ptr;
}

void __drop(void * ptr) {
    if (ptr == NULL) {
        return;
    }
    i64 * counter = (i64 *) ptr;
    i64 * bitset = (i64 *) ptr + 8;
    // void * vtable = ptr + 16;
    void ** fields = (void **) ptr + 24;

    switch(*counter) {
        case -1: // const string
            return;
        case 1:
            for (size_t i = 0; i < 64; i++) {
                if (*bitset & (1u << i)) {
                    __drop(*(fields + i * sizeof(i64))); // dobry offset?
                }
            }
            free(ptr);
            return;
        default:
            *counter -= 1;
            return;
    }
}