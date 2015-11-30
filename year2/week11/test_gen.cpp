#include <set>
#include <iostream>
#include <random>
#include <vector>

void gen_expr(std::vector<char> ops, int diff, std::string expr, int depth = 10) {
    if (depth == 0)
        return;
    if (diff == 1)
        std::cout << expr << std::endl;

    if (diff > 1)
        gen_expr(ops, diff - 1, expr + " " + ops[rand() % ops.size()], depth - 1);

    gen_expr(ops, diff + 1, expr + " " + std::to_string(rand() % 200), depth - 1);
}

int main() {
    srand(time(NULL));
    std::vector<char> ops = {'+', '-', '*', '/'};
    std::string expr = std::to_string(rand() % 200) + " " + std::to_string(rand() % 200) + " " + ops[rand() % ops.size()];
    std::cout << 23 << std::endl;
    gen_expr(ops, 1, expr, 20);
    return 0;
}
