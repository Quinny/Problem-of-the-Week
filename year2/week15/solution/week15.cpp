#include <vector>
#include <iostream>

// A type alias to save me from typing
// vector<vector<T>> everytime
template <typename T>
using matrix = std::vector<std::vector<T>>;

// Create an n x m matrix filled with f
template <typename T>
matrix<T> make (std::size_t n, std::size_t m, T f = 0) {
    matrix<T> ret;
    for (std::size_t i = 0; i < n; ++i)
        // Emplace back constructs the item in place
        // saves from potentially calling constructors
        // more than needed
        ret.emplace_back(m, f);
    return ret;
}

int main() {
    std::size_t n, m;
    std::cin >> n >> m;

    auto shelf = make(n, m, 0);
    for (std::size_t i = 0; i < n; ++i) {
        for (std::size_t j = 0; j < m; ++j)
            std::cin >> shelf[i][j];
    }

    // On the far right and bottom edges there is only
    // one possible path (down and right respectively)
    for (int i = n - 2; i >= 0; --i) {
        shelf[n - 1][i] += shelf[n - 1][i + 1];
        shelf[i][m - 1] += shelf[i + 1][m - 1];
    }

    for (int i = n - 2; i >= 0; --i) {
        for (int j = m - 2; j >= 0; --j)
            shelf[i][j] += std::max(shelf[i][j + 1], shelf[i + 1][j]);
    }

    std::cout << shelf[0][0] << std::endl;
    return 0;
}
