#include <stdio.h>

size_t strlen(const char* s) {
    const char* sc;
    for (sc = s; *sc != '\0'; ++sc);
    return sc - s;
}

int main() {
    char cipher[255];
    scanf("%s", cipher);

    size_t len = strlen(cipher);
    for (long i = len - 1; i >= 0; --i) {
        int offset = i == 0 ? 'a' + 5 : cipher[i - 1];
        char tmp = (cipher[i] - offset) % 26;
        if (tmp < 0)
            tmp += 26;

        cipher[i] = 'a' + tmp;
    }

    printf("%s\n", cipher);
    return 0;
}
