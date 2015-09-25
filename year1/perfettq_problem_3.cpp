/*
 * Given a list of m integers, find all pairs that sum to an integer n, and display
 * them in ascending order according to the first number in the pair.
 *
 * The pairs should be generated in O(n) time, and the sorting can be done in
 * O(nlog(n)) time.
 */

#include <iostream>
#include <vector>
#include <unordered_map>
#include <queue>

struct Compare{
	bool operator() (std::pair<int, int> x, std::pair<int, int> y){ return x.first > y.first; }
};

int main(){
	std::unordered_map<int, int> p;
	std::priority_queue<std::pair<int, int>, std::vector<std::pair<int, int>>, Compare> pairs;
	std::vector<int> v;
	int m;
	std::cin >> m;
	for(int i = 0; i < m; i++){
		int x;
		std::cin >> x;
		v.push_back(x);
	}
	int n;
	std::cin >> n;
	for(auto i:v){
		if(p.find(i) != p.end()){
			pairs.push(std::make_pair(i, p[i]));
			pairs.push(std::make_pair(p[i], i));
		}
		else
			p[n - i] = i;
	}
	while(!pairs.empty()){
		std::pair<int, int> tmp = pairs.top();
		std::cout << tmp.first << " + " << tmp.second << std::endl;
		pairs.pop();
	}
	return 0;
}
