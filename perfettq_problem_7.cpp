/*
 * Given a list of integers where every element has a duplicate except one,
 * find the non duplicated element.
 *
 * Expected runtime: O(n)
 * Expected memory:  O(1)
 */

#include <iostream>
#include <vector>

int non_dup(std::vector<int> const&);

int main(){
    int n;
    std::cin >> n;
    std::vector<int> v;
    for(int i = 0; i < n; i++){
        int tmp;
        std::cin >> tmp;
        v.push_back(tmp);
    }
    std::cout << non_dup(v) << std::endl;
    return 0;
}

int non_dup(std::vector<int> const& v){
    int ans = v[0];
    for(int i = 1; i < v.size(); i++)
        ans ^= v[i];
    return ans;
}
