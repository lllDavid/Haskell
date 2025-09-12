#include <stdint.h>

int64_t array_sum(int64_t* arr, int n) {
    int64_t sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}