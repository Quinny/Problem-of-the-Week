#include "union_find.h"
#include <iostream>
#include <vector>

template <typename T>
using matrix = std::vector<std::vector<T>>;
using point = std::pair<int, int>;

bool between(int a, int x, int y) {
    return a >= x && a < y;
}

std::vector<point> adj(int x, int y) {
    std::vector<point> ret;
    for (int i = -1; i <= 1; ++i) {
        for (int j = -1; j <= 1; ++j)
            ret.push_back({x + i, y + j});
    }
    return ret;
}

int main() {
    matrix<char> map;
    int m, n;
    std::cin >> m >> n;
    for (int i = 0; i < m; ++i) {
        map.emplace_back(n, 0);
        for (int j = 0; j < n; ++j)
            std::cin >> map[i][j];
    }

    union_find<point> uf;
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (map[i][j] == '+') {
                for (auto n: adj(i, j)) {
                    if (between(n.first, 0, map.size()) &&
                            between(n.second, 0, map[0].size()) &&
                            map[n.first][n.second] == '+' &&
                            !uf.connected({i, j}, n)) {
                        uf.connect({i, j}, n);
                    }
                }
            }
        }
    }
    std::cout << uf.components << std::endl;
    return 0;
}
