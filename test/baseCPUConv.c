#include <stdio.h> 
#include <stdlib.h>
#include <string.h>

#define INPUT_SIZE 32
#define KERNEL_SIZE 1
#define PADDING (KERNEL_SIZE / 2)
#define OUTPUT_SIZE INPUT_SIZE 

unsigned long read_cycles(void) {
	unsigned long cycles;
	asm volatile ("rdcycle %0" : "=r" (cycles));
}

void convolve2D(int input[INPUT_SIZE][INPUT_SIZE],
                int kernel[KERNEL_SIZE][KERNEL_SIZE],
                int output[OUTPUT_SIZE][OUTPUT_SIZE]) {

    // Iterate over each output element
    for (int i = 0; i < OUTPUT_SIZE; i++) {
        for (int j = 0; j < OUTPUT_SIZE; j++) {
            int sum = 0;

            // Apply the kernel
            for (int ki = 0; ki < KERNEL_SIZE; ki++) {
                for (int kj = 0; kj < KERNEL_SIZE; kj++) {
                    int ii = i + ki - PADDING;
                    int jj = j + kj - PADDING;

                    // Check for padding 
                    if (ii >= 0 && ii < INPUT_SIZE && jj >= 0 && jj < INPUT_SIZE) {
                        sum += input[ii][jj] * kernel[ki][kj];
                    }
                }
            }
            output[i][j] = sum;
        }
    }
}

int main() {
    int input[INPUT_SIZE][INPUT_SIZE];
    int kernel[KERNEL_SIZE][KERNEL_SIZE];
    int output[OUTPUT_SIZE][OUTPUT_SIZE];

    // Initialise input
    for (int i = 0; i < INPUT_SIZE; i++) {
        for (int j = 0; j < INPUT_SIZE; j++) {
            input[i][j] = i;
        }
    }

    // Initialise kernel
    for (int i = 0; i < KERNEL_SIZE; i++) {
        for (int j = 0; j < KERNEL_SIZE; j++) {
            kernel[i][j] = 1;
        }
    }
    int start = read_cycles();
    convolve2D(input, kernel, output);
    int end = read_cycles();

    // Print output
    /*for (int i = 0; i < OUTPUT_SIZE; i++) {
        for (int j = 0; j < OUTPUT_SIZE; j++) {
            printf("%6d ", output[i][j]);
        }
        printf("\n");
    }*/

    printf("My convolution took %lu cycles\n", end-start);

    return 0;
}