#ifndef _QP_HASHSET_H_
#define _QP_HASHSET_H_

#include <stddef.h>
#include <stdlib.h>

double CAPACITY = 0.6;

typedef struct {
    size_t value;
    char taken;
} hashentry;

typedef struct {
    hashentry* table;
    size_t alloc;
    size_t size;
    size_t grow_at;
} hashset;

size_t size_hint(size_t n) {
    size_t shift = 1;
    --n;
    while ((n + 1) & n) {
        n |= n >> shift;
        shift <<= 1;
    }
    return n + 1;
}

size_t hash_fn(const hashset* const hs, size_t n) {
    return n & (hs->alloc - 1);
}

void he_ctor(hashentry* he) {
    he->value = 0;
    he->taken = 0;
}

hashset* hs_ctor(size_t hint) {
    hashset* hs = (hashset*)malloc(sizeof(hashset));
    size_t alloc = size_hint(hint);

    hs->table = (hashentry*)(malloc(sizeof(hashentry) * alloc));
    for (size_t i = 0; i < alloc; ++i)
        he_ctor(&hs->table[i]);
    hs->alloc = alloc;
    hs->size = 0;
    hs->grow_at = alloc * CAPACITY;

    return hs;
}

void hs_dtor(hashset* self) {
    free(self->table);
    free(self);
}

void hs_insert(hashset* const self, size_t v);

void hs_rehash(hashset* const self) {
    hashset* ns = hs_ctor((self->alloc * 2) + 1);

    for (size_t i = 0; i < self->alloc; ++i) {
        if (self->table[i].taken) {
            hs_insert(ns, self->table[i].value);
        }
    }

    self->alloc = ns->alloc;
    self->size = ns->size;
    self->grow_at = ns->grow_at;
    free(self->table);
    self->table = ns->table;
    free(ns);
}

void hs_insert(hashset* const self, size_t v) {
    if (self->size == self->grow_at)
        hs_rehash(self);
    size_t h = hash_fn(self, v);
    while (self->table[h].taken) {
        if (self->table[h].value == v)
            return;
        h = hash_fn(self, h + 1);
    }
    self->table[h].taken = 1;
    self->table[h].value = v;
    ++self->size;
}

char hs_contains(hashset* const self, size_t v) {
    size_t h = hash_fn(self, v);
    while (self->table[h].taken) {
        if (self->table[h].value == v)
            return 1;
        h = hash_fn(self, h + 1);
    }
    return 0;
}

#endif
