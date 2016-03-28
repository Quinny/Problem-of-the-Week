#include <stdio.h>
#include "qpfs.h"

#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))

void unmask(void* data, size_t data_len, const raw_t* key, size_t key_len) {
    raw_t* cdata = (raw_t*)data;

    int key_idx = 0;
    for (size_t i = 0; i < data_len; ++i) {
        cdata[i] ^= key[key_idx];
        key_idx = (key_idx + 1) % key_len;
    }
}

int blob_done(FILE* fp, size_t key_len, raw_t* key) {
    size_t test_sym;
    fread(&test_sym, sizeof(size_t), 1, fp);
    unmask(&test_sym, sizeof(size_t), key, key_len);
    if (test_sym == ~0ul)
        return 1;
    fseek(fp, sizeof(size_t) * -1, SEEK_CUR);
    return 0;
}

int main() {
    printf("%lu\n", sizeof(size_t));

    FILE* f = fopen("blob.qp", "rb");

    size_t key_len;
    fread(&key_len, sizeof(size_t), 1, f);
    printf("key len: %lu\n", key_len);

    raw_t key[key_len];
    fread(key, 1, key_len, f);

    while (!blob_done(f, key_len, key)) {
        char fname[255];
        fread(fname, 1, 255, f);
        unmask(fname, 255, key, key_len);
        printf("fname: %s\n", fname);

        size_t len;
        fread(&len, sizeof(size_t), 1, f);
        unmask(&len, sizeof(size_t), key, key_len);
        printf("file size: %lu\n", len);

        char buffer[len];
        fread(buffer, 1, len, f);
        unmask(buffer, len, key, key_len);
        printf("%s", buffer);
    }

    fclose(f);
}
