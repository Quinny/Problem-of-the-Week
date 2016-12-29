#include <iostream>
#include <queue>
#include <set>
#include <vector>

// A DogeNet is simply a matrix of connection costs.
using DogeNet = std::vector<std::vector<int>>;

// A special constant to denote that two Doge's are not connected.
const int NOT_CONNECTED = 0;

// A struct to represent the connection between two doges.
struct DogeConnection {
  int from, to, cost;
  DogeConnection(int f, int t, int c) : from(f), to(t), cost(c) {}

  bool operator<(const DogeConnection& rhs) const {
    return cost != rhs.cost ? cost < rhs.cost
                            : from != rhs.from ? from < rhs.from : to < rhs.to;
  }

  bool operator==(const DogeConnection& rhs) const {
    return from == rhs.from && to == rhs.to && cost == rhs.cost;
  }

  bool operator>(const DogeConnection& rhs) const {
    return !(*this < rhs) && !(*this == rhs);
  }
};

// A little convenience wrapper for checking if a given value belongs to a
// container.  Should work for any standard associative container.
template <typename Container>
bool contains(const Container& c, const typename Container::value_type& v) {
  return c.find(v) != c.end();
}

// Returns an NxN DogeNet with no connections.
DogeNet empty_dogenet(int n_doges) {
  return DogeNet(n_doges, std::vector<int>(n_doges, NOT_CONNECTED));
}

// Computes the cost of a given DogeNet.
int dogenet_cost(const DogeNet& net) {
  int cost = 0;
  // Since all DogeNets are symmetric, only the upper half of connections
  // need to be considered.
  for (auto i = 0ul; i < net.size(); ++i) {
    for (auto j = 0ul; j <= i; ++j) {
      cost += net[i][j];
    }
  }
  return cost;
}

// Computes an optimal DogeNet by removing connections from the original
// DogeNet. Connections are removed according to Prim's MST algorithm.
DogeNet optimal_dogenet(const DogeNet& original_network) {
  DogeNet optimal_network = empty_dogenet(original_network.size());
  std::priority_queue<DogeConnection, std::vector<DogeConnection>,
                      std::greater<DogeConnection>>
      pq;
  std::set<int> doges_in_optimal_network;

  for (auto i = 0ul; i < original_network.size(); ++i) {
    if (original_network[0][i] != NOT_CONNECTED) {
      pq.emplace(0, i, original_network[0][i]);
    }
  }

  while (doges_in_optimal_network.size() != original_network.size()) {
    auto least_cost_connection = pq.top();
    // Find the least cost connection not already considered in the optimal
    // dogenet.
    while (contains(doges_in_optimal_network, least_cost_connection.to)) {
      pq.pop();
      least_cost_connection = pq.top();
    }
    pq.pop();

    // Add this least cost connection to the optimal dogenet.
    doges_in_optimal_network.insert(least_cost_connection.to);
    doges_in_optimal_network.insert(least_cost_connection.from);

    optimal_network[least_cost_connection.from][least_cost_connection.to] =
        least_cost_connection.cost;
    optimal_network[least_cost_connection.to][least_cost_connection.from] =
        least_cost_connection.cost;

    // Add all outgoing connections from the new Doge to the heap of edges to
    // be considered.
    for (auto i = 0ul; i < original_network.size(); ++i) {
      if (original_network[least_cost_connection.to][i] != NOT_CONNECTED &&
          optimal_network[least_cost_connection.to][i] == NOT_CONNECTED) {
        pq.emplace(least_cost_connection.to, i,
                   original_network[least_cost_connection.to][i]);
      }
    }
  }

  return optimal_network;
}

int main() {
  int dogenet_size;
  std::cin >> dogenet_size;

  DogeNet net = empty_dogenet(dogenet_size);
  for (int i = 0; i < dogenet_size; ++i) {
    for (int j = 0; j < dogenet_size; ++j) {
      std::cin >> net[i][j];
    }
  }

  const auto optimal_net = optimal_dogenet(net);
  std::cout << dogenet_cost(optimal_net) << std::endl;
}
