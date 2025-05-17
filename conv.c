#include <stdio.h>

#define N 9
#define K 3

void conv(float input[N][N], float kernel[K][K], float output[N][N]) {
    int pad = K / 2;

    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            float sum = 0.0;
            for (int m = 0; m < K; m++) {
                for (int n = 0; n < K; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;

                    if (x >= 0 && x < N && y >= 0 && y < N) {
                        //printf("i = %d, j = %d, m = %d, n = %d, x = %d, y = %d | %f * %f\n", i, j, m, n, x, y, kernel[m][n], input[x][y]);
                        sum += kernel[m][n] * input[x][y];
                    }
                }
            }
            output[i][j] = sum;
            //printf("Sum = %f\n", sum);
        }
    }
}

void conv_center(float input[N+K-1][N+K-1], float kernel[K][K], float output[N][N]) {
    int pad = K / 2;

    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            float sum = 0.0;
            for (int m = 0; m < K; m++) {
                for (int n = 0; n < K; n++) {
                    sum += kernel[m][n] * input[i + m][j + n];
                }
            }
            output[i][j] = sum;
        }
    }
}


int main() {
    // Input 3x3 matrix
    //float input[N][N] = {
    //    {1, 2, 3},
    //    {4, 5, 6},
    //    {7, 8, 9}
    //};
    
    //float input1[N+K-1][N+K-1] = {
    //    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    //    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0},
    //    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    //    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    //};

    float input1[N+K-1][N+K-1] = {
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
};

    float input[N][N] = {
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9},
    {1, 2, 3, 4, 5, 6, 7, 8, 9}

};
    
    //float input[N][N] = {
    //    {1, 2, 3, 4, 5, 6, 7, 8},
    //    {1, 2, 3, 4, 5, 6, 7, 8},
    //    {1, 2, 3, 4, 5, 6, 7, 8},
    //    {1, 2, 3, 4, 5, 6, 7, 8},
    //    {1, 2, 3, 4, 5, 6, 7, 8},
    //    {1, 2, 3, 4, 5, 6, 7, 8},
    //    {1, 2, 3, 4, 5, 6, 7, 8},
    //    {1, 2, 3, 4, 5, 6, 7, 8}
    //};

    // 3x3 kernel (e.g., simple edge detection kernel)
    float kernel[K][K] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    
    
    //float kernel[K][K] = {
    //    {1, 2, 3, 4, 5},
    //    {6, 7, 8, 9, 10},
    //    {11, 12, 13, 14, 15},
    //    {16, 17, 18, 19, 20},
    //    {21, 22, 23, 24, 25}
    //};
    
    

    // Output 3x3 matrix
    float output[N][N];

    // Perform 2D convolution
    conv(input, kernel, output);
    // Print output matrix
    printf("Output:\n");
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            printf("%6.2f ", output[i][j]);
        }
        printf("\n");
    }


    conv_center(input1, kernel, output);

    // Print output matrix
    printf("Output 1:\n");
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            printf("%6.2f ", output[i][j]);
        }
        printf("\n");
    }
    return 0;
}