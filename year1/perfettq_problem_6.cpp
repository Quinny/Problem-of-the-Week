/*
 * Given a list of integers such that the i'th element represents the price
 * of a stock on the i'th day, output the maximum profit that can be made over the
 * time period.
 *
 * Note that you may only by 1 unit at a time, and must sell that unit in order
 * to buy again.
 *
 * Expected runtime: O(n)
 */

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
