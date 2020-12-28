#include <stdlib.h>
#include <stdio.h>
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