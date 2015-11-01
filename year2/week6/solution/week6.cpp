#include "directed_graph.h"
#include <vector>
#include <iostream>
#include <queue>

template <typename T>
std::vector<T> toposort(directed_graph<T> g) {
    std::vector<T> order;
    std::queue<T> q;

    for (auto i: g.incoming_count_) {
        if (i.second == 0)
            q.push(i.first);
    }

    while (!q.empty()) {
        auto t = q.front();
        q.pop();
        order.push_back(t);
        for (auto i: g[t]) {
            g.disconnect(t, i);
            if (g.incoming_count_[i] == 0)
                q.push(i);
        }
    }
    return order;
}

int main() {
    int n;
    std::cin >> n;
    directed_graph<std::string> g;
    while (n--) {
        std::string s1, s2;
        std::cin >> s1 >> s2;
        g.connect(s1, s2);
    }
    auto out = toposort(g);
    for (auto i: out)
        std::cout << i << " ";
    std::cout << std::endl;
    return 0;
}
