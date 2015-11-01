#include <iostream>
#include <fstream>
#include "solution/directed_graph.h"
#include <random>
#include <vector>
#include <set>
#include <queue>

template <typename T>
struct rand_vec {
    std::vector<T> v;

    void push_back(T x) {
        v.push_back(x);
    }

    T get() {
        return v[rand() % v.size()];
    }
};

directed_graph<std::string> fix(directed_graph<std::string> g) {
    directed_graph<std::string> ret;
    std::set<std::string> seen;
    std::queue<std::string> q;

    auto tmp = g.g_.begin()->first;
    q.push(tmp);
    seen.insert(tmp);

    while (!q.empty()) {
        auto t = q.front();
        q.pop();

        for (auto i: g[t]) {
            auto check = seen.insert(i);
            if (check.second) {
                q.push(i);
                ret.connect(t, i);
            }
        }
    }
    return ret;
}

directed_graph<std::string> make_dag(rand_vec<std::string> labels) {
    int edges = rand() % 1000;
    directed_graph<std::string> ret;

    while (edges--)
        ret.connect(labels.get(), labels.get());
    return fix(ret);
}

int main() {
    srand(time(nullptr));
    rand_vec<std::string> event_names;

    std::string s;
    while (std::cin >> s)
        event_names.push_back(s);

    for (int i = 0; i < 100; ++i) {
        std::ofstream out("tests/" + std::to_string(i) + ".test");
        auto g = make_dag(event_names);
        int edges = 0;

        for (auto i: g.g_) {
            for (auto j: i.second) {
                ++edges;
            }
        }
        out << edges << std::endl;
        for (auto i: g.g_) {
            for (auto j: i.second) {
                out << i.first << " " << j << std::endl;
            }
        }
        out.close();
    }
    return 0;
}
