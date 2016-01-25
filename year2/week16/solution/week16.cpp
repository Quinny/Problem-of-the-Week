#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <functional>

// Represents current tile, and current move
using turn_state = std::pair<int, int>;

template <typename Node, typename Expand, typename Done>
Node search(Node s, Expand e, Done d) {
    std::queue<Node> q;
    std::set<Node> seen;

    q.push(s);
    while (!q.empty()) {
        auto t = q.front();
        q.pop();

        if (d(t))
            return t;

        auto ex = e(t);
        for (auto i: ex) {
            auto check = seen.insert(i);
            if (check.second)
                q.push(i);
        }
    }
}

std::vector<turn_state> expand(turn_state ts, std::map<int, int> tiles) {
    std::vector<turn_state> ret;
    for (int i = 1; i <= 6; ++i) {
        int next;
        if (tiles[ts.first + i])
            next = tiles[ts.first + i];
        else
            next = ts.first + i;
        if (next <= 100)
            ret.push_back({next, ts.second + 1});
    }
    return ret;
}

bool game_over(turn_state ts) {
    return ts.first == 100;
}

int main() {
    int n;
    std::cin >> n;

    std::map<int, int> tiles;
    int start, end;
    while (std::cin >> start >> end)
        tiles[start] = end;

    using namespace std::placeholders;
    auto exf = std::bind(expand, _1, tiles);
    std::cout << search(turn_state(1, 0), exf, game_over).second << std::endl;
}
