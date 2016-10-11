#include <iostream>
#include <unordered_map>

// A data structure for efficiently managing disjoint sets.  The disjoint sets
// are represented as trees, where the root of the tree is known as the set's
// representative.  All children of the representative belong to the same set.
// Two useful operations are provided:
//  Union - Join two subsets into a single set.  This is done by finding the
//          representative for both sets, u and v, and then connecting u to be
//          a child of v.
//
//  Find  - Find the representative for the given element.
//
// Note that A belongs to the same set as B if they have the same
// representative.
template <typename T>
class UnionFind {
 public:
  // Merge the sets containing u and v.
  void connect(const T& u, const T& v) {
    add_if_not_present(u);
    add_if_not_present(v);
    if (u == v) return;

    // Find the represntative of each element.
    const auto& r1 = find(u);
    const auto& r2 = find(v);

    // Attach the smaller of the two trees to be a child of the larger.  This
    // keeps the tree's shallow.
    if (rank_[r1] >= rank_[r2]) {
      parent_[r2] = r1;
      rank_[r1] += rank_[r2];
    } else {
      parent_[r1] = r2;
      rank_[r2] += rank_[r1];
    }
  }

  // Check if u and v belong to the same set.
  bool connected(const T& u, const T& v) {
    if (parent_.find(u) == parent_.end() || parent_.find(v) == parent_.end()) {
      return false;
    }

    return find(u) == find(v);
  }

 private:
  // Map a node to its parent node.  If parent_[a] == a, then a is a root.
  std::unordered_map<T, T> parent_;
  // Map a node to its number of child nodes.
  std::unordered_map<T, std::size_t> rank_;

  // Create a new singleton set if u is not in any other set.
  void add_if_not_present(const T& u) {
    if (parent_.find(u) == parent_.end()) {
      parent_[u] = u;
      rank_[u] = 1;
    }
  }

  // Find the representative of u.  This find method also performs path
  // compression such that the next time it is called with the same argument,
  // the look up should be constant.  This method assumes that u is contained
  // in a set.
  const T& find(const T& u) {
    const auto& u_parent = parent_.find(u)->second;
    if (u_parent == u) {
      return u;
    }
    return parent_[u] = find(u_parent);
  }
};

int main() {
  std::ios_base::sync_with_stdio(false);

  int n_friendships;
  std::cin >> n_friendships;

  UnionFind<std::string> squads;
  std::string n1, n2;
  for (int i = 0; i < n_friendships; ++i) {
    std::cin >> n1 >> n2;
    squads.connect(n1, n2);
  }

  int n_queries;
  std::cin >> n_queries;

  for (int i = 0; i < n_queries; ++i) {
    std::cin >> n1 >> n2;
    std::cout << (squads.connected(n1, n2) ? "yes" : "no") << std::endl;
  }
}
