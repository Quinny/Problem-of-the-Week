#include <iostream>
#include <memory>
#include <unordered_map>

template <typename T>
struct TrieNode {
  std::unordered_map<T, std::unique_ptr<TrieNode<T>>> children;
  bool word_end;

  TrieNode() : word_end(false) {}

  TrieNode* get_or_create_child(const T& x) {
    auto it_bool = children.emplace(x, nullptr);
    if (it_bool.second) {
      it_bool.first->second.reset(new TrieNode<T>());
    }
    return it_bool.first->second.get();
  }

  const TrieNode* get_or_null(const T& x) const {
    const auto it = children.find(x);
    return it == children.end() ? nullptr : it->second.get();
  }
};

template <typename T>
class Trie {
 public:
  Trie() : root_(new TrieNode<T>()) {}

  template <typename Iter>
  void insert(Iter first, Iter last) {
    auto* current = root_.get();
    while (first != last) {
      current = current->get_or_create_child(*first);
      ++first;
    }
    current->word_end = true;
  }

  template <typename Iter>
  bool any_prefix(Iter first, Iter last) const {
    const auto* current = root_.get();
    while (first != last) {
      if (current->word_end) return true;
      current = current->get_or_null(*first);
      if (current == nullptr) return false;
      ++first;
    }
    return current->word_end;
  }

 private:
  std::unique_ptr<TrieNode<T>> root_;
};

int main() {
  int n_prefixes;
  std::cin >> n_prefixes;

  Trie<char> trie;
  std::string ip;
  for (int i = 0; i < n_prefixes; ++i) {
    std::cin >> ip;
    ip.push_back('.');

    trie.insert(ip.begin(), ip.end());
  }

  int n_requests;
  std::cin >> n_requests;
  for (int i = 0; i < n_requests; ++i) {
    std::cin >> ip;
    ip.push_back('.');

    std::cout << (trie.any_prefix(ip.begin(), ip.end()) ? "banned" : "valid")
              << std::endl;
  }
}
