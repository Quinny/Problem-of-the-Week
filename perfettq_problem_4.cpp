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