#include <iostream>
#include <vector>

int max_profit(std::vector<int> const&);

int main(){
    std::vector<int> stocks;
    int n, tmp;
    std::cin >> n;
    for(int i = 0; i < n; i++){
        std::cin >> tmp;
        stocks.push_back(tmp);
    }
    std::cout << max_profit(stocks) << std::endl;
    return 0;
}

int max_profit(std::vector<int> const& stocks){
    int profit = 0;
    for(int i = 0; i < stocks.size() - 1; i++){
        if(stocks[i] < stocks[i + 1]) profit += stocks[i + 1] - stocks[i];
    }
    return profit;
}
