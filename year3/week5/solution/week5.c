#include <stdio.h>
#include <stdlib.h>

// https://en.wikipedia.org/wiki/Trie

// A node in a bit trie.  Each node has two children and an end of "word"
// marker.
struct bit_trie_node {
  struct bit_trie_node* children[2];
  char end;
};

typedef struct bit_trie_node bit_trie_node;

// A pruned trie is just like a regular trie except that the nodes are pruned
// such that only leaf nodes are word ends.
//
// Consider the case where there are two malicious IP prefixes:
//
//  1.1
//  1.1.234
//
// If the first prefix is already present in the trie, the second one is not
// needed.
typedef struct { bit_trie_node* root; } pruned_bit_trie;

// Allocate memory for a bit trie node and initilize the members.
bit_trie_node* btn_ctor() {
  bit_trie_node* btn = (bit_trie_node*)malloc(sizeof(bit_trie_node));
  btn->children[0] = NULL;
  btn->children[1] = NULL;
  btn->end = 0;

  return btn;
}

// Free the memory for the given node, and all of its descendants.
void btn_dtor(bit_trie_node* btn) {
  if (btn == NULL) return;
  btn_dtor(btn->children[0]);
  btn_dtor(btn->children[1]);
  free(btn);
}

// Initialize a pruned bit trie (essentially just allocating the root node).
void pbt_init(pruned_bit_trie* pbt) { pbt->root = btn_ctor(); }

// Free the memory for a pruned bit trie.
void pbt_deinit(pruned_bit_trie* pbt) { btn_dtor(pbt->root); }

//  Given a node and a child index, return the corresponding child.  If the
//  child does not yet exist, it will be allocated.
bit_trie_node* get_or_allocate_child(bit_trie_node* btn, const unsigned int c) {
  if (btn->children[c] == NULL) {
    btn->children[c] = btn_ctor();
  }
  return btn->children[c];
}

// Insert the |bits| least significant bits of |n| into the pruned bit trie.
void pbt_insert(pruned_bit_trie* pbt, unsigned int n, unsigned int bits) {
  bit_trie_node* current_node = pbt->root;
  for (/* empty */; bits != 0; --bits, n >>= 1) {
    // If we come across a word end while inserting stop.  This keeps the tree
    // pruned.
    if (current_node->end) return;
    current_node = get_or_allocate_child(current_node, n & 1);
  }
  current_node->end = 1;

  // Prune everything after this node to keep the trie as shallow and
  // non-redundant as possible.
  btn_dtor(current_node->children[0]);
  btn_dtor(current_node->children[1]);
  current_node->children[0] = NULL;
  current_node->children[1] = NULL;
}

// Determines if any "words" in the trie are a prefix of the bits of n.  Bit
// sequences that are equal are treated as being a prefix of one another.
int pbt_prefix_any(const pruned_bit_trie* pbt, unsigned int n) {
  const bit_trie_node* current_node = pbt->root;
  for (/* empty */; n != 0; n >>= 1) {
    if (current_node->end) return 1;
    current_node = current_node->children[n & 1];
    if (current_node == NULL) return 0;
  }
  return 1;
}

// Converts a potentially partial IP address in dotted octet format into its
// unsigned integer representation.  The number of bits in the partial IP
// address will be stored in the |bits| variable.
unsigned int partial_ip_to_integer(const char* buffer, unsigned int* bits) {
  unsigned int ip = 0;
  unsigned int offset = 0;

  while (*buffer != 0) {
    unsigned int mask = 0;
    // Build an integer representing the current octet.
    while (*buffer >= '0' && *buffer <= '9') {
      mask = (10 * mask) + (*buffer - '0');
      ++buffer;
    }

    // Mask the octet into the IP address.
    ip |= mask << offset;
    offset += 8;

    if (*buffer == '.') ++buffer;
  }

  *bits = offset;
  return ip;
}

int main() {
  pruned_bit_trie pbt;
  pbt_init(&pbt);

  // 4 octets maximum 3 characters each, 3 dots, 1 null terminator.
  char ip_buffer[(4 * 3) + 3 + 1];
  unsigned int bits;

  int n_prefixes;
  scanf("%d", &n_prefixes);
  for (int i = 0; i < n_prefixes; ++i) {
    scanf("%s", ip_buffer);
    unsigned int ip = partial_ip_to_integer(ip_buffer, &bits);
    pbt_insert(&pbt, ip, bits);
  }

  int n_ips;
  scanf("%d", &n_ips);
  for (int i = 0; i < n_ips; ++i) {
    scanf("%s", ip_buffer);
    unsigned int ip = partial_ip_to_integer(ip_buffer, &bits);
    if (pbt_prefix_any(&pbt, ip)) {
      printf("banned\n");
    } else {
      printf("valid\n");
    }
  }

  pbt_deinit(&pbt);
}
