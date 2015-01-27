#include <iostream>
#include <vector>

template <typename iter, typename val_type = int>
val_type kth_smallest(iter, iter, iter, iter, int);
std::vector<int> read_vector();

int main(){
   auto a = read_vector();
   auto b = read_vector();
   int k;
   std::cin >> k;
   std::cout << kth_smallest(a.begin(), a.end(), b.begin(), b.end(), k - 1) << std::endl;
}

std::vector<int> read_vector(){
    int n;
    std::cin >> n;
    std::vector<int> v(n);
    for(int i = 0; i < n; i++){
        int tmp;
        std::cin >> tmp;
        v[i] = tmp;
    }
    return v;
}

template <typename iter, typename val_type>
val_type kth_smallest(iter first1, iter last1, iter first2, iter last2, int k){
    if(first1 == last1) return *(first2 + k);
    if(first2 == last2) return *(first1 + k);
    
    int mid1 = (last1 - first1) / 2;
    int mid2 = (last2 - first2) / 2;
    if(mid1 + mid2 < k){
        // Logic cherry picked from linear time order-statistic algorithm
        // if we find that the kth smallest is not in the current section
        // then we must look ahead, and adjust k
        if(*(first2 + mid2) < *(first1 + mid1))
            return kth_smallest(first1, last1, first2 + mid2 + 1, last2, k - mid2 - 1);
        else
            return kth_smallest(first1 + mid1 + 1, last1, first2, last2, k - mid1 - 1);
    }
    else{
        // otherwise, it is in the current section, so no adjusting of k is nessesary
        if(*(first2 + mid2) < *(first1 + mid1))
            return kth_smallest(first1, first1 + mid1, first2, last2, k);
        else
            return kth_smallest(first1, last1, first2, first2 + mid2, k);
    }
}

