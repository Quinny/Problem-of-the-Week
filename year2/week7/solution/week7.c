#include "hashset.h"
#include <stdio.h>
#include <stdlib.h>

void print(hashentry he) {
    if (he.occurs > 1)
        printf("%zu\n", he.value);
}

int main() {
    hashset* hs = hs_ctor(129);
    int n;
    scanf("%d%*c", &n);
    while (n--) {
        int tmp;
        scanf("%d%*c", &tmp);
        hs_insert(hs, tmp);
    }
    hs_for_each(hs, print);
    hs_dtor(hs);
    return 0;
}
