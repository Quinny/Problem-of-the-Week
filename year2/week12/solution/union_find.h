#include <map>

template <typename T>
struct union_find {
    std::map<T, T> parents;
    std::map<T, std::size_t> sizes;
    std::size_t components = 0;

    void init_vertex(const T& u) {
        if (parents.find(u) == parents.end()) {
            parents[u] = u;
            sizes[u] = 1;
            ++components;
        }
    }

    T find_root(T u) {
        while (parents[u] != u)
            u = parents[u];
        return u;
    }

    void connect(const T& a, const T& b) {
        init_vertex(a);
        init_vertex(b);
        if (a == b)
            return;

        auto r1 = find_root(a);
        auto r2 = find_root(b);

        if (sizes[r1] >= sizes[r2]) {
            parents[r2] = r1;
            sizes[r1] += sizes[r2];
        }
        else {
            parents[r1] = r2;
            sizes[r2] += sizes[r1];
        }
        --components;
    }

    bool connected(T u, T v) {
        if (parents.find(u) == parents.end() || parents.find(v) == parents.end())
            return false;
        return find_root(u) == find_root(v);
    }
};
