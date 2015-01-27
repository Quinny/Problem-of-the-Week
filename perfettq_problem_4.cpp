/*
 * two characters a and b are said to be adjacent if they differ by exactly 1 bit
 * ex. 1001 and 1011 are adjacent, where as 1001 and 0101 are not.
 *
 * Given 2 characters a and b, check whether they are adjacent
 * Note if a and b are the same, they are NOT adjacent
 *
 * Expected runtime: O(1)
 */

#include <iostream>

bool adjacent(char, char);
bool pow_2(char);

int main(){
	char a, b;
	std::cin >> a >> b;
	std::cout << adjacent(a, b) << std::endl;
	return 0;
}

bool adjacent(char a, char b){
	if(a == b) return false;
	return pow_2(a ^ b);
}

bool pow_2(char a){
	return (a & (a - 1)) == 0;
}
