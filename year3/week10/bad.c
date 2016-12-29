#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  char name[20];
  int max_password_len = 20;
  char secret[20] = "password123";
  char password[20];
  int authenticated = 0;

  printf("What is your name? ");
  scanf(" %[^\n]%*c", name);

  // Use fgets to prevent buffer overflow.
  printf("Password: ");
  fgets(password, max_password_len, stdin);

  if (strcmp(password, secret) == 0) {
    authenticated = 1;
  }

  if (authenticated) {
    printf("Weclome %s, here is all the passwords\n", name);
    system("cat /etc/shadow");
  }
}
