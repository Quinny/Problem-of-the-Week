#ifndef _QP_HASHSET_H_
#define _QP_HASHSET_H_

#include <stddef.h>
#include <stdlib.h>

// A dynamic size hashset in C
// One thing to note:
// - The table size ALWAYS a power of 2.
// Reason:
// Generally the way to insure the output of your hash function is within
// [0, alloc - 1] is to mod by alloc.  Mod is a relatively slow operation.
// To get around this, we keep our table size a power of 2 (i.e. 10* in binary)
// and simply bitwise AND the output of the hash function with (alloc - 1).
// This will clear the most significant n bits (where alloc = 2^n) and insure
// that your hash is within [0, alloc - 1].  This method also gives better
// distribution than modding.

// Maximum load factor for hash set
double CAPACITY = 0.6;

// An entry in our hashset
typedef struct {
    size_t value;
    char taken;
    size_t occurs;
} hashentry;

// The hashset itself
typedef struct {
    // table of entries
    hashentry* table;
    // total allocated size of table
    size_t alloc;
    // number of taken entries
    size_t size;
    // once size == grow_at, rehash
    size_t grow_at;
} hashset;

// callback used for iteration
typedef void (*he_callback)(hashentry he);

// finds the next power of 2 > n
size_t size_hint(size_t n) {
    size_t shift = 1;
    --n;
    while ((n + 1) & n) {
        n |= n >> shift;
        shift <<= 1;
    }
    return n + 1;
}

// use our fancy trick to avoid modding
size_t hash_fn(const hashset* const hs, size_t n) {
    return n & (hs->alloc - 1);
}

// Constructors
void he_ctor(hashentry* he) {
    he->value = 0;
    he->taken = 0;
    he->occurs = 0;
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

// hashset destructor
void hs_dtor(hashset* self) {
    free(self->table);
    free(self);
}

void hs_insert(hashset* const self, size_t v);

// once our load capacity is reached, rehash all values and increase
// table size by a power of 2
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

// check if rehashing is needed, and insert new value
void hs_insert(hashset* const self, size_t v) {
    if (self->size == self->grow_at)
        hs_rehash(self);
    size_t h = hash_fn(self, v);
    while (self->table[h].taken) {
        if (self->table[h].value == v) {
            ++self->table[h].occurs;
            return;
        }
        h = hash_fn(self, h + 1);
    }
    self->table[h].taken = 1;
    self->table[h].value = v;
    self->table[h].occurs = 1;
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

// iterate through all taken buckets
void hs_for_each(hashset* const self, he_callback cb) {
    for (size_t i = 0; i < self->alloc; ++i) {
        if (self->table[i].taken) {
            cb(self->table[i]);
        }
    }
}

#endif
