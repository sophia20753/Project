#include <stdio.h>
#include <stdint.h>

#define N3 3
#define N5 5
#define N8 8
#define N10 10
#define N12 12
#define N32 32


#define K5 5
#define K3 3
#define K1 1

void conv81(float input[N8][N8], float kernel[K1][K1], float output[N8][N8]) {
    int pad = K1 / 2;

    for (int i = 0; i < N8; i++) {
        for (int j = 0; j < N8; j++) {
            float sum = 0.0;
            for (int m = 0; m < K1; m++) {
                for (int n = 0; n < K1; n++) {
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

void conv323(float input[N32][N32], float kernel[K3][K3], float output[N32][N32]) {
    int pad = K3 / 2;

    for (int i = 0; i < N32; i++) {
        for (int j = 0; j < N32; j++) {
            float sum = 0.0;
            for (int m = 0; m < K3; m++) {
                for (int n = 0; n < K3; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N32 && y >= 0 && y < N32) {
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

void conv325(float input[N32][N32], float kernel[K5][K5], float output[N32][N32]) {
    int pad = K5 / 2;

    for (int i = 0; i < N32; i++) {
        for (int j = 0; j < N32; j++) {
            float sum = 0.0;
            for (int m = 0; m < K5; m++) {
                for (int n = 0; n < K5; n++) {
                    int x = i + m - pad;
                    int y = j + n - pad;
                    if (x >= 0 && x < N32 && y >= 0 && y < N32) {
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

    float input8[N8][N8] = {
        {1,2,3,4,5,6,7,8},
        {1,2,3,4,5,6,7,8},
        {1,2,3,4,5,6,7,8},
        {1,2,3,4,5,6,7,8},
        {1,2,3,4,5,6,7,8},
        {1,2,3,4,5,6,7,8},
        {1,2,3,4,5,6,7,8},
        {1,2,3,4,5,6,7,8}
    };

    float input10[N10][N10] = {
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1},
        {1,2,3,4,5,6,7,8,9,1}
    };

    float input12[N12][N12] = {
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,2,3,4,5,6,7,8,9,9},
        {1,1,1,1,1,1,1,1,1,1,1,1},
        {1,1,1,1,1,1,1,1,1,1,1,1}
    };

    float input32[N32][N32];
    
    // 3x3 kernel (e.g., simple edge detection kernel)
    float kernel3[K3][K3] = {
        {1.0,  5.0, -1.0},
        {2.0,  0.5, -2.0},
        {3.0,  0.5, -3.0}
    };
    
    
    float kernel5[K5][K5] = {
        {1.0,  5.0, -1.0,  0.5,  1.0},
        {2.0,  0.5, -2.0,  0.5,  2.0},
        {3.0,  0.5, -3.0,  0.5,  3.0},
        {2.0,  0.5, -2.0,  0.5,  2.0},
        {1.0,  0.5, -1.0,  0.5,  1.0}
    };

    float kernel1[K1][K1] = {
        {1.5}
    };
    
    

    // Output 3x3 matrix
    float output81[N8][N8];

    float output103[N10][N10];

    float output125[N12][N12];

    float output323[N32][N32];

    float output325[N32][N32];

    // Setup input

    for (int i = 0; i < N32; i++) {
        for (int j = 0; j < N32; j++) {
            input32[i][j] = (float)((i*N32 + j) % 4); // Example input data
        }
    }

    // Perform 2D convolution
    conv81(input8, kernel1, output81);
    conv103(input10, kernel3, output103);
    conv125(input12, kernel5, output125);
    conv323(input32, kernel3, output323);
    conv325(input32, kernel5, output325);

    // Print output matrix

    printf("Output 81:\n");
    for (int i = 0; i < N8; i++) {
        for (int j = 0; j < N8; j++) {
            //printf("%6.0f  ", output8[i][j]);
            int16_t fx = (int16_t)(output81[i][j] * 256.0f);
            printf("%04x  ", (uint16_t)fx);
        }
        printf("\n");
    }

    printf("Output 105:\n");
    for (int i = 0; i < N10; i++) {
        for (int j = 0; j < N10; j++) {
            //printf("%6.0f  ", output103[i][j]);
            int16_t fx = (int16_t)(output103[i][j] * 256.0f);
            printf("%04x  ", (uint16_t)fx);
        }
        printf("\n");
    }

    printf("Output 125:\n");
    for (int i = 0; i < N12; i++) {
        for (int j = 0; j < N12; j++) {
            //printf("%6.0f  ", output125[i][j]);
            int16_t fx = (int16_t)(output125[i][j] * 256.0f);
            printf("%04x  ", (uint16_t)fx);
        }
        printf("\n");
    }

    printf("Output 323:\n");
    for (int i = 0; i < N32; i++) {
        for (int j = 0; j < N32; j++) {
            printf("%6.0f  ", output323[i][j]);
        }
        printf("\n");
    }

    printf("Output 323 (8x8 tiles):\n");
    for (int i = 0; i < N32/8; i++) {
        for (int j = 0; j < N32/8; j++) {
            printf("Tile i: %d, j: %d\n", i, j);
            for (int x = i*8; x < (i+1)*8; x++) {
                for (int y = j*8; y < (j+1)*8; y++) {
                    int16_t fx = (int16_t)(output323[x][y] * 256.0f);
                    printf("%04x  ", (uint16_t)fx);
                }
                printf("\n");
            }
            printf("\n");
        }
    }



    printf("Output 325 (8x8 tiles):\n");
    for (int i = 0; i < N32/8; i++) {
        for (int j = 0; j < N32/8; j++) {
            printf("Tile i: %d, j: %d\n", i, j);
            for (int x = i*8; x < (i+1)*8; x++) {
                for (int y = j*8; y < (j+1)*8; y++) {
                    int16_t fx = (int16_t)(output325[x][y] * 256.0f);
                    printf("%04x  ", (uint16_t)fx);
                }
                printf("\n");
            }
            printf("\n");
        }
    }


    return 0;
}