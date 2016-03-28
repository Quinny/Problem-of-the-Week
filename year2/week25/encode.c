#include <stdio.h>
#include <string.h>
#include "qpfs.h"

int main(int argc, char* argv[]) {
    const char* path = argv[1];
    const char* key_s  = argv[2];

    size_t key_len = strlen(key_s);
    raw_t key[key_len];
    memcpy(key, key_s, key_len);

    qpfs_blob b;
    qpfs_blob_init(&b, path, key, key_len);

    for (int i = 3; i < argc; ++i)
        qpfs_blob_addf(&b, argv[i]);

    qpfs_blob_end(&b);
    qpfs_blob_free(&b);
}
