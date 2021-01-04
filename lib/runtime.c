#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>

#define string char *

void error() {
    fprintf(stderr, "runtime error\n");
    exit(1);
}

void printInt(const int i) {
    printf("%d", i);
}

void printString(const string s) {
    printf("%s", s);
}

int readInt() {
    int read;
    scanf("%d", &read);
    return read;
}

string readString() {
    return readline(NULL);
}

string __strcat(const string s1, const string s2) {
    size_t s1_len = strlen(s1);
    size_t s2_len = strlen(s2);

    size_t buf_len = s1_len + s2_len + 1;
    string buf = (string) malloc(buf_len);

    memcpy(buf, s1, s1_len);
    memcpy(buf, s2, s2_len);
    buf[buf_len - 1] = 0;

    return buf;
}