#include <map>
#include <functional>
#include <sstream>
#include <stack>
#include <iostream>

template <typename T>
using binary_op = std::function<T(T, T)>;

using op_map = std::map<char, binary_op<int>>;

int evaluate(std::string expr, op_map& ops) {
    std::string term;
    std::stringstream ss(expr);
    std::stack<int> s;

    do {
        ss >> term;
        if (std::isalnum(term.front()))
            s.push(std::stoi(term));
        else {
            if (s.size() == 1)
                break;
            auto n2 = s.top(); s.pop();
            auto n1 = s.top(); s.pop();
            s.push(ops[term.front()](n1, n2));
        }
    } while(!s.empty());

    return s.top();
}

int main() {
    op_map ops;
    ops['+'] = [](int x, int y) { return x + y; };
    ops['*'] = [](int x, int y) { return x * y; };
    ops['/'] = [](int x, int y) { return x / y; };
    ops['-'] = [](int x, int y) { return x - y; };

    int n;
    std::cin >> n;
    std::cin.get();
    while (n--) {
        std::string expr;
        std::getline(std::cin, expr);
        std::cout << evaluate(expr, ops) << std::endl;
    }
    return 0;
}
