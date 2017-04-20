#include <iostream>
#include <vector>

using Tree = std::vector<std::vector<int>>;

bool is_lucky(const Tree& t, unsigned row = 0, unsigned col = 0,
              int target = 13) {
  bool is_leaf = row == t.size();
  if (is_leaf) {
    return target == 0;
  }

  return is_lucky(t, row + 1, 2 * col, target - t[row][col]) ||
         is_lucky(t, row + 1, 2 * col + 1, target - t[row][col]);
}

int main() {
  std::ios_base::sync_with_stdio(false);

  int n_rows;
  std::cin >> n_rows;

  Tree tree(n_rows);
  for (int i = 0; i < n_rows; ++i) {
    tree[i].resize(1 << i);
    for (int j = 0; j < 1 << i; ++j) {
      std::cin >> tree[i][j];
    }
  }

  std::cout << (is_lucky(tree) ? "lucky" : "not lucky") << std::endl;
}
