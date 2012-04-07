#include <iostream>
#include <algorithm>
using namespace std;

int main() {
    int a[] = {1, 2, 5, 10, 20, 50, 100, 200}, n = 8;
    int dp[201]; fill(dp, dp + 201, 0); dp[0] = 1;
    for (int i = 0; i != 8; ++i) {
        for (int sum = a[i]; sum <= 200; ++sum) {
            dp[sum] += dp[sum - a[i]];
        }
    }
    cout << dp[200] << endl;
    return 0;
}