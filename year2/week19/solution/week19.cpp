#include <iostream>
#include <algorithm>
#include <iterator>
#include "trie.h"

// A trie_node is invalid if it is an end node
// and not a leaf.  This indicates that their exists a "word" in the trie
// which is a prefix of another
template <typename T>
bool invalid_end(qp::trie_node<T> n) {
    return n.end && !n.children.empty();
}

int main() {
    // Read strings from stdin and insert them into the trie
    qp::trie<std::basic_string, char> t(
            std::istream_iterator<std::string>(std::cin),
            std::istream_iterator<std::string>()
        );

    // If any of the nodes are found to be invalid, return false
    if (std::any_of(t.begin(), t.end(), invalid_end<char>))
        std::cout << "False" << std::endl;
    else
        std::cout << "True" << std::endl;
    return 0;
}
