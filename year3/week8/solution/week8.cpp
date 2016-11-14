#include <iostream>
#include <random>
#include <vector>

namespace {

// A utility class which provides uniformly distributed random numbers seeded
// with the hash function on a given input.  Useful for generating multiple
// bloomfilter bit indexes for a key using only a single hash function.
template <typename T, typename Hash = std::hash<T>>
struct Mixer {
  std::minstd_rand rng_;
  std::size_t max_;
  Mixer(const T& val, std::size_t m) : rng_(Hash()(val)), max_(m) {}
  std::size_t operator()() { return rng_() % max_; }
};

}  // namespace

// A probabilistic space efficient data structure used for testing membership in
// a set.
// https://en.wikipedia.org/wiki/Bloom_filter
template <typename Key, int NumHashes, typename Hash = std::hash<Key>>
class Bloomfilter {
 public:
  Bloomfilter(std::size_t size) : bits_(size, false) {}

  std::size_t size() const { return bits_.size(); }

  void add(const Key& key) {
    Mixer<Key, Hash> mixer(key, bits_.size());
    for (int i = 0; i < NumHashes; ++i) {
      bits_[mixer()] = true;
    }
  }

  bool maybe_contains(const Key& key) const {
    Mixer<Key, Hash> mixer(key, bits_.size());
    for (int i = 0; i < NumHashes; ++i) {
      if (!bits_[mixer()]) return false;
    }
    return true;
  }

 private:
  // In c++, a vector<bool> is actually a bit array.
  std::vector<bool> bits_;
};

int main() {
  std::ios_base::sync_with_stdio(false);

  int n;
  std::cin >> n;

  Bloomfilter<std::string, 10> bf(n * 10);

  std::string url;
  for (int i = 0; i < n; ++i) {
    std::cin >> url;
    bf.add(url);
  }

  int m;
  std::cin >> m;
  for (int i = 0; i < m; ++i) {
    std::cin >> url;
    std::cout << (bf.maybe_contains(url) ? "maybe malicious" : "not malicious")
              << std::endl;
  }
}
