#ifndef STRBUF_H
#define STRBUF_H

/*
 * A helper for handling dynamically resizing
 * buffers of chars.  (a.k.a. strings)
 *
 * All buffers will be null char delimited
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

typedef struct {
    // the amount of space allocated
    size_t alloc;
    // the amount of space currently being used
    size_t len;
    // a pointer to the memory
    char *buf;
} strbuf;

// initialize the buffer with a given allocation size
void strbuf_init(strbuf * const self, size_t hint) {
    self->alloc = hint > 0 ? hint : 1;
    self->len = 0;
    self->buf = (char*)malloc(sizeof(char) * hint);
    self->buf[0] = '\0';
}

// clean up the memory
void strbuf_free(strbuf * const self) {
    free(self->buf);
    self->alloc = 0;
    self->len = 0;
    self->buf = NULL;
}

// grow the buffer by size n if needed
void __strbuf_grow_by(strbuf * const self, size_t n) {
    if (self->alloc < self->len + n) {
        self->buf = (char*)realloc(self->buf, self->alloc + n);
        self->alloc += n;
    }
}

// append a string to the end of the buffer
void strbuf_addstr(strbuf * const self, const char* s) {
   size_t n = strlen(s);
   __strbuf_grow_by(self, n + 1);
    strcpy(self->buf + self->len, s);
    self->len += n;
}

// append a char to the end of the buffer
void strbuf_addch(strbuf * const self, const char c) {
    __strbuf_grow_by(self, 2);
    self->buf[self->len++] = c;
    self->buf[self->len] = '\0';
}

// join strings together with sep and append the result to the buffer
// separated so that it can be called with a va_list
void __strbuf_join_impl(strbuf* const self, int n, char sep, va_list strs) {
    for (int i = 0; i < n; ++i) {
        strbuf_addstr(self, va_arg(strs, const char*));
        if (i + 1 != n)
            strbuf_addch(self, sep);
    }
}

void strbuf_addstrjoin(strbuf* const self, int n, char sep, ...) {
    va_list strs;
    va_start(strs, sep);
    __strbuf_join_impl(self, n, sep, strs);
    va_end(strs);
}

// sets the length of the current buffer and delimits it accordingly
// useful for "clearing" the buffer and reusing its already allocated space
// e.x. strbuf_setlen(&s, 0); => cleared buffer
void strbuf_setlen(strbuf* const self, size_t n) {
    if (n > self->len)
        __strbuf_grow_by(self, n - self->len);
    self->len = n;
    self->buf[n] = '\0';
}


#endif /* STRBUF_H */
