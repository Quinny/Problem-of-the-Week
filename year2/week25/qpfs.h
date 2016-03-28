#ifndef QPFS_H
#define QPFS_H

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "file.h"

typedef unsigned char raw_t;

typedef struct {
    FILE* fp;
    raw_t* key;
    size_t key_len;
} qpfs_blob;


void mask(void* data,
          size_t data_len,
          const raw_t* key,
          size_t key_len,
          raw_t* out) {
    raw_t* cdata = (raw_t*)data;
    int key_idx = 0;
    for (size_t i = 0; i < data_len; ++i) {
        out[i] = cdata[i] ^ key[key_idx];
        key_idx = (key_idx + 1) % key_len;
    }
}
void qpfs_blob_init(qpfs_blob * const self,
        const char* path, raw_t* key, size_t key_len) {
   self->fp = fopen(path, "wb");
   self->key = (raw_t*)malloc(key_len);
   memcpy(self->key, key, key_len);
   self->key_len = key_len;

   // write key to file
   fwrite(&key_len, sizeof(size_t), 1, self->fp);
   fwrite(key, 1, self->key_len, self->fp);
}

void qpfs_blob_addf(qpfs_blob * const self, const char* path) {
    qp_file f = qpf_open(path);

    raw_t enc_name[255];
    mask(f.name, 255, self->key, self->key_len, enc_name);
    fwrite(enc_name, 1, 255, self->fp);

    raw_t enc_len[sizeof(size_t)];
    mask(&f.contents.len, sizeof(size_t), self->key, self->key_len, enc_len);
    fwrite(enc_len, 1, sizeof(size_t), self->fp);

    raw_t enc_contents[f.contents.len];
    mask(f.contents.buf, f.contents.len, self->key, self->key_len, enc_contents);
    fwrite(enc_contents, f.contents.len, 1, self->fp);

    qpf_close(&f);
}

void qpfs_blob_end(qpfs_blob * const self) {
    size_t x = -1;
    mask(&x, sizeof(size_t), self->key, self->key_len, (void*)&x);
    fwrite(&x, 1, sizeof(size_t), self->fp);
}

void qpfs_blob_free(qpfs_blob * const self) {
    fclose(self->fp);
    free(self->key);
    self->key_len = 0;
}

#endif /* QPFS_H */
