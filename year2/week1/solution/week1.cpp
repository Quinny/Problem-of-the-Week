#include <iostream>
#include <map>
#include <vector>

int common_bits(unsigned int n, unsigned int m) {
    int ret = 0;
    for (unsigned int diff = n ^ m; diff != 0; diff >>= 1)
        ret += diff & 1;
    return ret;
}

int main() {
    int n;
    std::cin >> n;
    std::vector<std::pair<std::string, unsigned int>> users;
    for (int i = 0; i < n; ++i) {
        std::string name;
        unsigned int value;
        std::cin >> name >> value;
        users.push_back({name, value});
    }

    int max = -1, min = 33;
    std::pair<std::string, std::string> alike, opposite;
    for (unsigned i = 0; i < users.size(); ++i) {
        for (unsigned j = i + 1; j < users.size(); ++j) {
            auto c = common_bits(users[i].second, users[j].second);
            if (c > max) {
                alike = {users[i].first, users[j].first};
                max = c;
            }
            if (c < min) {
                opposite = {users[i].first, users[j].first};
                min = c;
            }
        }
    }

    std::cout << opposite.first << " " << opposite.second << std::endl;
    std::cout << alike.first << " " << alike.second << std::endl;

    return 0;
}
