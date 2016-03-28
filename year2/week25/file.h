#ifndef FILE_H
#define FILE_H

#include <stdio.h>
#include "strbuf.h"

#define MAX_NAME_LENGTH 255

typedef struct {
    char name[MAX_NAME_LENGTH];
    strbuf contents;
} qp_file;

void qpf_init(qp_file * const self) {
    self->name[0] = '\0';
    strbuf_init(&self->contents, 100);
}

qp_file qpf_open(const char* path) {
    FILE* fp = fopen(path, "r");
    char buffer[512];
    qp_file f;
    qpf_init(&f);

    strcpy(f.name, path);
    int got;
    while (got = fread(buffer, 1, 510, fp), got) {
        buffer[got] = '\0';
        strbuf_addstr(&f.contents, buffer);
    }
    fclose(fp);
    return f;
}

void qpf_close(qp_file * const self) {
    strbuf_free(&self->contents);
}

#endif /* FILE_H */
