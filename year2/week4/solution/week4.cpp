#include <vector>
#include <iostream>

using point = std::pair<int, int>;

template <typename T>
using matrix = std::vector<std::vector<T>>;

struct sumdata {
    point start, end;
    int sum;
};

sumdata kadane(const std::vector<int> v) {
    int current = v[0];
    int start = 0;
    sumdata ret = {
        {0, 0},
        {0, 0},
        current
    };
    for (std::size_t i = 1; i < v.size(); ++i) {
        if (current < 0) {
            current = v[i];
            start = i;
        }
        else
            current += v[i];
        if (current > ret.sum) {
            ret.start.second = start;
            ret.end.second = i;
            ret.sum = current;
        }
    }
    return ret;
}

sumdata two_d_kadane(const matrix<int>& m) {
    sumdata ret = {
        {0, 0},
        {0, 0},
        m[0][0]
    };
    for (std::size_t left = 0; left < m[0].size(); ++left) {
        std::vector<int> tmp(m.size(), 0);
        for (std::size_t right = left; right < m[0].size(); ++right) {
            for (std::size_t i = 0; i < m.size(); ++i)
                tmp[i] += m[i][right];
            auto check = kadane(tmp);
            if (check.sum > ret.sum) {
                std::cout << check.sum << std::endl;
                ret.start.first = check.start.second;
                ret.start.second = left;

                ret.end.first = check.end.second;
                ret.end.second = right;

                ret.sum = check.sum;
            }
        }
    }
    return ret;
}

std::ostream& operator << (std::ostream& os, point p) {
    os << "(" << p.first << ", " << p.second << ")";
    return os;
}

int main() {
    int n, m;
    std::cin >> n >> m;

    matrix<int> field;
    for (int i = 0; i < n; ++i) {
        std::vector<int> v;
        for (int j = 0; j < m; ++j) {
            int tmp;
            std::cin >> tmp;
            v.push_back(tmp);
        }
        field.push_back(v);
    }

    auto v = two_d_kadane(field);
    std::cout << v.start << " " << v.end << " " << v.sum << std::endl;

    return 0;
}
