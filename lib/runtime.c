#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define string char *
#define BUF_SIZE 256

static char buf[BUF_SIZE];

void error() {
    fprintf(stderr, "runtime error\n");
    exit(1);
}

void printInt(const long i) {
    printf("%ld\n", i);
}

void printString(const string s) {
    printf("%s\n", s);
}

long readInt() {
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