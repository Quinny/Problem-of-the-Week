#include <iostream>
#include <unordered_map>
#include <vector>

std::vector<int> read_vector();
int pair_count(std::vector<int> const&, int const k);

int main() {
    auto v = read_vector();
    int k;
    std::cin >> k;
    std::cout << pair_count(v, k) << std::endl;
    return 0;
}

int pair_count(std::vector<int> const& v, int const k) {
    std::unordered_map<int, int> m;
    int ret = 0;
    for (auto i : v) {
        if (m.find(i) != m.end())
            ++ret;
        m[k + i] = i;
    }
    return ret;
}

std::vector<int> read_vector() {
    std::vector<int> v;
    int n, tmp;
    std::cin >> n;
    while (n--) {
        std::cin >> tmp;
        v.push_back(tmp);
    }
    return v;
}
