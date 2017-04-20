#include <iostream>
#include <iterator>
#include <numeric>
#include <queue>
#include <set>
#include <unordered_map>
#include <vector>

using Gene = std::string;

// A simple graph class which uses an adjacency map to represent connections.
// a => {b, c} implies that a has an outgoing edge to both b and c.
template <typename T>
class Graph {
 public:
  void connect(const T& u, const T& v) { adjacency_map_[u].insert(v); }

  void connect_bidirectional(const T& u, const T& v) {
    connect(u, v);
    connect(v, u);
  }

  const std::set<T>& operator[](const T& u) const {
    const auto maybe_nodeset = adjacency_map_.find(u);
    return maybe_nodeset == adjacency_map_.end() ? kEmptyNodeSet
                                                 : maybe_nodeset->second;
  }

 private:
  std::unordered_map<T, std::set<T>> adjacency_map_;
  static std::set<T> kEmptyNodeSet;
};

// Static.
template <typename T>
std::set<T> Graph<T>::kEmptyNodeSet;

// Two genes are siblings iff their genome strings differ by a single character.
// In other words, two genes are siblings if the hamming distance between them
// is 1.
bool siblings(const Gene& g1, const Gene& g2) {
  // Sum all the character pairs which are not equal.
  return std::inner_product(g1.begin(), g1.end(), g2.begin(), 0,
                            std::plus<int>(), std::not_equal_to<char>()) == 1;
}

// Check if a container holds a value following c++'s iterator pattern.
template <typename Container, typename Value>
bool contains(const Container& c, const Value& v) {
  return c.find(v) != c.end();
}

// Determine the degrees of separation between two nodes in a graph.  If they
// are not connected -1 is returned.
template <typename T>
int degrees_of_separation(const T& start, const T& target, const Graph<T>& g) {
  std::queue<std::pair<T, int>> to_explore;
  std::set<T> explored;

  std::unordered_map<T, T> prev;

  to_explore.emplace(start, 0);
  explored.insert(start);

  while (!to_explore.empty()) {
    const auto current = to_explore.front();
    to_explore.pop();

    if (current.first == target) return current.second;

    for (const auto& neighbour : g[current.first]) {
      if (!contains(explored, neighbour)) {
        to_explore.emplace(neighbour, current.second + 1);
        explored.insert(neighbour);
        prev[neighbour] = current.first;
      }
    }
  }

  return -1;
}

int main() {
  Gene start_gene;
  int n_genes;
  std::cin >> start_gene >> n_genes;

  std::vector<Gene> known_genes(std::istream_iterator<Gene>{std::cin},
                                std::istream_iterator<Gene>{});

  Graph<Gene> mutation_graph;
  for (auto i = 0ul; i < known_genes.size(); ++i) {
    for (auto j = i + 1; j < known_genes.size(); ++j) {
      if (siblings(known_genes[i], known_genes[j])) {
        mutation_graph.connect_bidirectional(known_genes[i], known_genes[j]);
      }
    }
  }

  std::cout << degrees_of_separation<Gene>(start_gene, "GEEK", mutation_graph)
            << std::endl;
}
