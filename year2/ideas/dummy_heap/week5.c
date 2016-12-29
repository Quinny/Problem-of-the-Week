#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char head;
    char alloc;
    size_t len;
} memory_block;

typedef struct {
    memory_block* memory;
    size_t len;
} memory_heap;

void memory_block_init(memory_block* b) {
    b->head = 0;
    b->alloc = 0;
    b->len = 0;
}

void memory_block_free(memory_block* b) {
    b->head = 0;
    b->alloc = 0;
    b->len = 0;
}

void memory_heap_init(memory_heap* h, size_t n) {
    h->memory = (memory_block*)malloc(sizeof(memory_block) * n);
    h->len = n;
    for (size_t i = 0; i < h->len; ++i)
        memory_block_init(&h->memory[i]);
}

void memory_heap_free(memory_heap* h) {
    for (size_t i = 0; i < h->len; ++i)
        memory_block_free(&h->memory[i]);
    free(h->memory);
    h->memory = NULL;
    h->len = 0;
}

int memory_heap_alloc(memory_heap* h, size_t start, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        if (h->memory[start + i].alloc == 1)
            return 0;
    }
    h->memory[start].head = 1;
    h->memory[start].len = n;
    for (size_t i = 0; i < n; ++i)
        h->memory[start + i].alloc = 1;
    return 1;
}

int memory_heap_delete(memory_heap* h, size_t addr) {
    // if we try and free a block that is not a head, then return
    if (h->memory[addr].head == 0)
        return 0;
    // get the block size, and free up the memory
    size_t block_size = h->memory[addr].len;
    for (size_t i = 0; i < block_size; ++i)
        memory_block_free(&h->memory[addr + i]);
    return 1;
}

int main() {
    // size of heap
    size_t n;
    scanf("%lu", &n);
    memory_heap h;
    memory_heap_init(&h, n);
    // number of commands
    size_t m;
    scanf("%lu", &m);

    char command[255];
    size_t arg1, arg2;
    while (1) {
        scanf("%s", command);
        if (strcmp(command, "quit") == 0)
            break;
        if (strcmp(command, "allocate") == 0) {
            scanf("%lu %lu ", &arg1, &arg2);
            int ret = memory_heap_alloc(&h, arg1, arg2);
            if (!ret)
                printf("allocation error at address 0x%08zx\n", arg1);
        }
        if (strcmp(command, "delete") == 0) {
            scanf("%lu ", &arg1);
            int ret = memory_heap_delete(&h, arg1);
            if (!ret)
                printf("delete error at address 0x%08zx\n", arg1);
        }
    }

    memory_heap_free(&h);
}
