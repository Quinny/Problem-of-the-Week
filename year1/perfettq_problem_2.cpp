/* Given 2 sorted arrays, find the median between the 2 as if they were merged.
 * Note that the arrays can have different lengths
 *
 * Expected runtime: O(log(m + n))
 */

#include <iostream>
#include <algorithm>

double findMedian(int[], int, int[], int);
int kthElement(int[], int, int, int[], int, int, int);

int main(void){
	int n, m;
	std::cin >> n >> m;
	int a[n];
	int b[m];
	for(int i = 0; i < n ; i++)
		std::cin >> a[i];
	for(int i = 0; i < m; i++)
		std::cin >> b[i];
	std::cout << findMedian(a, n, b, m) << std::endl;
	return 0;
}

double findMedian(int a[], int n, int b[], int m){
	if((n + m) % 2 == 0)
		return (kthElement(a, 0, n - 1, b, 0, m - 1, (m + n) / 2) + kthElement(a, 0, n - 1, b, 0, m - 1, (m + n) / 2 - 1)) / 2.0;
	else
		return kthElement(a, 0, n - 1, b, 0, m - 1, (m + n) / 2);
}

int kthElement(int a[], int alo, int ahi, int b[], int blo, int bhi, int k){
	if(ahi < alo)
		return b[blo + k];
	if(bhi < blo)
		return a[alo + k];
	if(k == 0)
		return std::min(a[alo], b[blo]);
	int amid = (ahi - alo + 1) * k / ((ahi - alo + 1) + (bhi - blo + 1));
	int bmid = k - amid - 1;
	amid = amid + alo;
	bmid = bmid + blo;
	if(a[amid] > b[bmid]){
		k -= bmid - blo + 1;
		ahi = amid;
		blo = bmid + 1;
	}
	else{
		k -= amid - alo + 1;
		bhi = bmid;
		alo = amid + 1;
	}
	return kthElement(a, alo, ahi, b, blo, bhi, k);
}
