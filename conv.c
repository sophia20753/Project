#include <stdio.h>

#define N3 3
#define N5 5
#define N8 8
#define N10 10
#define N12 12


#define K5 5
#define K3 3

void conv33(float input[N3][N3], float kernel[K3][K3], float output[N3][N3]) {
    int pad = K3 / 2;

    for (int i = 0; i < N3; i++) {
        for (int j = 0; j < N3; j++) {
            float sum = 0.0;
            for (int m = 0; m < K3; m++) {
                for (int n = 0; n < K3; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N3 && y >= 0 && y < N3) {
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

void conv83(float input[N8][N8], float kernel[K3][K3], float output[N8][N8]) {
    int pad = K3 / 2;

    for (int i = 0; i < N8; i++) {
        for (int j = 0; j < N8; j++) {
            float sum = 0.0;
            for (int m = 0; m < K3; m++) {
                for (int n = 0; n < K3; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N8 && y >= 0 && y < N8) {
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

void conv103(float input[N10][N10], float kernel[K3][K3], float output[N10][N10]) {
    int pad = K3 / 2;

    for (int i = 0; i < N10; i++) {
        for (int j = 0; j < N10; j++) {
            float sum = 0.0;
            for (int m = 0; m < K3; m++) {
                for (int n = 0; n < K3; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N10 && y >= 0 && y < N10) {
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

void conv55(float input[N5][N5], float kernel[K5][K5], float output[N5][N5]) {
    int pad = K5 / 2;

    for (int i = 0; i < N5; i++) {
        for (int j = 0; j < N5; j++) {
            float sum = 0.0;
            for (int m = 0; m < K5; m++) {
                for (int n = 0; n < K5; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N5 && y >= 0 && y < N5) {
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

void conv85(float input[N8][N8], float kernel[K5][K5], float output[N8][N8]) {
    int pad = K5 / 2;

    for (int i = 0; i < N8; i++) {
        for (int j = 0; j < N8; j++) {
            float sum = 0.0;
            for (int m = 0; m < K5; m++) {
                for (int n = 0; n < K5; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N8 && y >= 0 && y < N8) {
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

void conv125(float input[N12][N12], float kernel[K5][K5], float output[N12][N12]) {
    int pad = K5 / 2;

    for (int i = 0; i < N12; i++) {
        for (int j = 0; j < N12; j++) {
            float sum = 0.0;
            for (int m = 0; m < K5; m++) {
                for (int n = 0; n < K5; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N12 && y >= 0 && y < N12) {
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

int main() {
    // Input 3x3 matrix
    float input3[N3][N3] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };

    float input5[N5][N5] = {
        {1, 2, 3, 4, 5},
        {1, 2, 3, 4, 5},
        {1, 2, 3, 4, 5},
        {1, 2, 3, 4, 5},
        {1, 2, 3, 4, 5}
    };

    float input8[N8][N8] = {
        {1, 2, 3, 4, 5, 6, 7, 8},
        {1, 2, 3, 4, 5, 6, 7, 8},
        {1, 2, 3, 4, 5, 6, 7, 8},
        {1, 2, 3, 4, 5, 6, 7, 8},
        {1, 2, 3, 4, 5, 6, 7, 8},
        {1, 2, 3, 4, 5, 6, 7, 8},
        {1, 2, 3, 4, 5, 6, 7, 8},
        {1, 2, 3, 4, 5, 6, 7, 8}
    };

    float input10[N10][N10] = {
        {0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0},
        {0,0,1,2,3,4,5,6,7,8},
        {0,0,1,2,3,4,5,6,7,8},
        {0,0,1,2,3,4,5,6,7,8},
        {0,0,1,2,3,4,5,6,7,8},
        {0,0,1,2,3,4,5,6,7,8},
        {0,0,1,2,3,4,5,6,7,8},
        {0,0,1,2,3,4,5,6,7,8},
        {0,0,1,2,3,4,5,6,7,8}
    };

    float input12[N12][N12] = {
        {0,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,1,2,3,4,5,6,7,8},
        {0,0,0,0,1,2,3,4,5,6,7,8},
        {0,0,0,0,1,2,3,4,5,6,7,8},
        {0,0,0,0,1,2,3,4,5,6,7,8},
        {0,0,0,0,1,2,3,4,5,6,7,8},
        {0,0,0,0,1,2,3,4,5,6,7,8},
        {0,0,0,0,1,2,3,4,5,6,7,8},
        {0,0,0,0,1,2,3,4,5,6,7,8}
    };
    
    // 3x3 kernel (e.g., simple edge detection kernel)
    float kernel3[K3][K3] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    
    
    float kernel5[K5][K5] = {
        {1, 2, 3, 4, 5},
        {6, 7, 8, 9, 10},
        {11, 12, 13, 14, 15},
        {16, 17, 18, 19, 20},
        {21, 22, 23, 24, 25}
    };
    
    

    // Output 3x3 matrix
    float output33[N3][N3];
    float output83[N8][N8];
    float output103[N10][N10];
    float output55[N5][N5];
    float output85[N8][N8];
    float output125[N12][N12];

    // Perform 2D convolution
    conv33(input3, kernel3, output33);
    conv83(input8, kernel3, output83);
    conv103(input10, kernel3, output103);
    conv55(input5, kernel5, output55);
    conv85(input8, kernel5, output85);
    conv125(input12, kernel5, output125);

    // Print output matrix
    printf("Output 33:\n");
    for (int i = 0; i < N3; i++) {
        for (int j = 0; j < N3; j++) {
            printf("%6.0f ", output33[i][j]);
        }
        printf("\n");
    }

    printf("Output 83:\n");
    for (int i = 0; i < N8; i++) {
        for (int j = 0; j < N8; j++) {
            printf("%6.0f ", output83[i][j]);
        }
        printf("\n");
    }

    printf("Output 103:\n");
    for (int i = 0; i < N10; i++) {
        for (int j = 0; j < N10; j++) {
            printf("%6.0f ", output103[i][j]);
        }
        printf("\n");
    }

    printf("Output 55:\n");
    for (int i = 0; i < N5; i++) {
        for (int j = 0; j < N5; j++) {
            printf("%6.0f ", output55[i][j]);
        }
        printf("\n");
    }

    printf("Output 85:\n");
    for (int i = 0; i < N8; i++) {
        for (int j = 0; j < N8; j++) {
            printf("%6.0f ", output85[i][j]);
        }
        printf("\n");
    }

    printf("Output 125:\n");
    for (int i = 0; i < N12; i++) {
        for (int j = 0; j < N12; j++) {
            printf("%6.0f ", output125[i][j]);
        }
        printf("\n");
    }


    return 0;
}