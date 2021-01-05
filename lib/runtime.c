#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define string char *
#define BUF_SIZE 256

char buf[BUF_SIZE];

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
    if(buf[BUF_SIZE - 2] == '\n') {
        buf[BUF_SIZE - 2] = '\0';
    }

    size_t len = strlen(buf);
    string read = (string) malloc(len + 1);
    memcpy(read, buf, len + 1);
    return read;
}

string __strcat(const string s1, const string s2) {
    size_t s1_len = strlen(s1);
    size_t s2_len = strlen(s2);

    size_t buf_len = s1_len + s2_len + 1;
    string buf = (string) malloc(buf_len);

    memcpy(buf, s1, s1_len);
    memcpy(buf + s1_len, s2, s2_len);
    buf[buf_len - 1] = 0;

    return buf;
}