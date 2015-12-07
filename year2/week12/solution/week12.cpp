#include <iostream>
#include <vector>
#include <queue>
#include <set>


template <typename T>
// type alias for a vector<vector<>> to reduce typing
using matrix = std::vector<std::vector<T>>;
using point = std::pair<int, int>;

// checks if a given point will fall inside of a matrix of size
// h x w
bool on_board(point p, int w, int h) {
    return p.first >= 0 && p.second >= 0 && p.second < w && p.first < h;
}

// translates a point by a direction vector
point translate(point p1, point p2) {
    return {p1.first + p2.first, p1.second + p2.second};
}

// returns all neighbouring points that fall within
// the width and height
std::vector<point> neighbours(point p, int w, int h) {
    std::vector<point> ret;
    for (int i = -1; i <= 1; ++i) {
        for (int j = -1; j <= 1; ++j) {
            auto np = translate(p, {i, j});
            if (on_board(np , w, h))
                ret.push_back(np);
        }
    }
    return ret;
}

// Flood fill on a matrix, visit all adjacent instances of target
// and replace them with rep
template <typename T>
void floodfill(matrix<T>& board, point p, const T& target, const T& rep) {
    // visit the current spot
    board[p.first][p.second] = rep;
    for (auto i: neighbours(p, board[0].size(), board.size())) {
        if (board[i.first][i.second] == target) {
            board[i.first][i.second] = rep;
            // call floodfill on all adjacent squares
            // which contain our target
            floodfill(board, i, target, rep);
        }
    }
}

int main() {
    int m, n;
    std::cin >> m >> n;
    matrix<char> map;

    for (int i = 0; i < m; ++i) {
        map.emplace_back(n, 0);
        for (int j = 0; j < n; ++j)
            std::cin >> map[i][j];
    }

    int ret = 0;
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (map[i][j] == '+') {
                ++ret;
                floodfill(map, {i, j}, '+', '.');
            }
        }
    }
    std::cout << ret << std::endl;
    return 0;
}
