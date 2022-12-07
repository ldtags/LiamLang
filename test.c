#include <stdio.h>
#include <math.h>

int main(int argc, char * argv[]) {
    int low;
    int mid;
    int high;
    int temp;
    int count;
    
    printf("Enter the number of triples to be processed: ");
    scanf("%d", &count);
    while (count > 0) {
        printf("Enter three integers: ");
        scanf("%d%d%d", &low, &mid, &high);
        if (low < mid && mid > high ) {
            temp = mid;
            mid = high;
            high = temp;
            if (low > mid) {
                temp = low;
                low = mid;
                mid = temp;
            }
        } else {
            if (low > mid && mid < high) {
                temp = low;
                low = mid;
                mid = temp;
                if (mid > high) {
                    temp = mid;
                    mid = high;
                    high = temp;
                }
            } else {
                if (low > mid && mid > high) {
                    temp = low;
                    low = high;
                    high = temp;
                }
            }
        }
        printf("%d %d %d\n", low, mid, high);
        count = count - 1;
    }

    
}
