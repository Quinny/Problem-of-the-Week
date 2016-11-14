#include <iostream>
#include <iterator>
#include <vector>

template <typename Node, typename AdvanceFn>
Node get_cycle_start(const Node& start, const AdvanceFn& advance) {
  auto tortoise = start;
  auto hare = start;

  do {
    tortoise = advance(tortoise);
    hare = advance(advance(hare));
  } while (tortoise != hare);

  hare = start;
  while (tortoise != hare) {
    tortoise = advance(tortoise);
    hare = advance(hare);
  }

  return tortoise;
}

int main() {
  int n;
  std::cin >> n;

  const std::vector<int> process_ids{std::istream_iterator<int>(std::cin),
                                     std::istream_iterator<int>()};

  const auto agent = get_cycle_start(
      process_ids.back(),
      [&process_ids](const int x) { return process_ids[x - 1]; });
  std::cout << agent << std::endl;
}
