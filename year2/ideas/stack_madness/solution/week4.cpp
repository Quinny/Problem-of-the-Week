#include <vector>
#include <iostream>

bool sum_exists(int n, int target, const std::vector<int>& possibles,
        std::vector<int> sums) {
    if (n == 0)
        return find(sums.begin(), sums.end(), target) != sums.end();
    std::vector<int> ret;
    for (auto i: possibles) {
        for (auto j: sums)
            ret.push_back(i + j);
    }
    return sum_exists(n - 1, target, possibles, ret);
}


bool sum_exists(int n, int target, const std::vector<int>& v) {
    auto copy = v;
    auto copy2 = v;
    return sum_exists(n - 1, target, copy, copy2);
}

int main() {
    int sum, n, m, tmp;
    std::vector<int> v;
    std::cin >> sum >> n >> m;
    for (int i = 0; i < m; ++i) {
        std::cin >> tmp;
        v.push_back(tmp);
    }
    std::cout << sum_exists(n, sum, v) << std::endl;
    return 0;
}
