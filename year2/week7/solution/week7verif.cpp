#include <iostream>
#include <set>

int main() {
    int n;
    std::set<int> s;

    std::cin >> n;
    while (n--) {
        int tmp;
        std::cin >> tmp;
        auto check = s.insert(tmp);
        if (!check.second)
            std::cout << tmp << std::endl;
    }
    return 0;
}
