#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "rocc.h"


#define KERNEL_SIZE 3
#define KERNEL_LEN (KERNEL_SIZE * KERNEL_SIZE)
#define PACKED_KERNEL_LEN ((KERNEL_LEN + 3) / 4) // 4 values per 64-bit word


#define INPUT_SIZE 32
#define OUTPUT_SIZE 32

#define INPUT_LEN (INPUT_SIZE * INPUT_SIZE)
#define OUTPUT_LEN (OUTPUT_SIZE * OUTPUT_SIZE)

#define OUTPUT_TILE_SIZE 8
#define OUTPUT_TILE_LEN (OUTPUT_TILE_SIZE * OUTPUT_TILE_SIZE)
#define PACKED_OUTPUT_TILE_LEN ((OUTPUT_TILE_LEN + 3) / 4) // 4 values per 64-bit word
#define INPUT_TILE_SIZE (OUTPUT_TILE_SIZE + KERNEL_SIZE - 1)
#define INPUT_TILE_LEN (INPUT_TILE_SIZE * INPUT_TILE_SIZE)
#define PACKED_INPUT_TILE_LEN ((INPUT_TILE_LEN + 3) / 4) // 4 values per 64-bit word


#define CUSTOM_OPCODE 0
#define FUNCT7_DOLOADLINPUT 0x01
#define FUNCT7_DOLOADKERNEL 0x02 // 0b0000010
#define FUNCT7_DOCOMPUTE 0x03

enum TileType {
    TOP_LEFT = 0,
    TOP = 1,
    TOP_RIGHT = 2,
    LEFT = 3,
    CENTER = 4,
    RIGHT = 5,
    BOTTOM_LEFT = 6,
    BOTTOM = 7,
    BOTTOM_RIGHT = 8
};

static inline uint64_t rdcycle() {

	uint64_t cycles;
	asm volatile ("rdcycle %0" : "=r" (cycles));
	return cycles;
}



static inline uint64_t doLoadKernel(uint64_t kernel_ptr, uint64_t kernel_size) {
    uint64_t result;
    // ROCC_INSTRUCTION_DSS(opcode, rd, rs1, rs2, funct7)
    // rs1 = kernel address
    // rs2 = kernel size
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, kernel_ptr, kernel_size, FUNCT7_DOLOADKERNEL);
    return result;
}

static inline uint64_t InputLoad(uint64_t input_ptr, uint64_t X) {
    uint64_t result;
    // ROCC_INSTRUCTION_DSS(opcode, rd, rs1, rs2, funct7)
    // rs1 = kernel address
    // rs2 = kernel size
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, input_ptr, 0, FUNCT7_DOLOADLINPUT);
    return result;
}

static inline uint64_t doCompute(uint64_t ptr, uint64_t tileType) {
    uint64_t result;
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, ptr, tileType, FUNCT7_DOCOMPUTE);
    return result;
}

uint16_t float_to_fixed88(float value) {
    int32_t fixed = (value * 256.0f);  // scale float to 8.8
    if (fixed < -32768 || fixed > 32767) {
        fprintf(stderr, "Error: value %.4f out of range for 8.8 fixed-point\n", value);
        exit(1);
    }
    return (uint16_t)(fixed & 0xFFFF);  // two's complement, lower 16 bits
}

uint16_t fixed88_to_float(uint16_t fixed) {
    int32_t value = (int32_t)(fixed & 0xFFFF);  // two's complement, lower 16 bits
    return (uint16_t)(value / 256.0f);  // convert back to float
}


// Kernel definition based on size
#if KERNEL_SIZE == 1
static float kernel_data[KERNEL_SIZE][KERNEL_SIZE] = {
    {1.5}
};
#elif KERNEL_SIZE == 3
static float kernel_data[KERNEL_SIZE][KERNEL_SIZE] = {
    {1.0,  5.0, -1.0},
    {2.0,  0.5, -2.0},
    {3.0,  0.5, -3.0}
};
#elif KERNEL_SIZE == 5
static float kernel_data[KERNEL_SIZE][KERNEL_SIZE] = {
    {1.0,  5.0, -1.0,  0.5,  1.0},
    {2.0,  0.5, -2.0,  0.5,  2.0},
    {3.0,  0.5, -3.0,  0.5,  3.0},
    {2.0,  0.5, -2.0,  0.5,  2.0},
    {1.0,  0.5, -1.0,  0.5,  1.0}
};
#else
#error "Unsupported KERNEL_SIZE. Must be 1, 3, or 5."
#endif

int main() {

    float input[INPUT_LEN];
    float input_tile[INPUT_TILE_LEN];
    uint64_t input_tile_packed[PACKED_INPUT_TILE_LEN];
    
    float output[OUTPUT_LEN];
    float output_tile[OUTPUT_TILE_LEN];
    uint16_t output_tile_f88[OUTPUT_TILE_LEN];
    uint64_t output_tile_packed[PACKED_OUTPUT_TILE_LEN];
    
    uint64_t packed_kernel_data[PACKED_KERNEL_LEN];

    for (int i = 0; i < INPUT_LEN; i++) {
        input[i] = (float)(i % 32); // Example input data
    }
    for (int i = 0; i < OUTPUT_LEN; i++) {
        output[i] = 0; // Example input tile data
    }
    
    // kernel processing
    for (int w = 0; w < PACKED_KERNEL_LEN; w++) {
        packed_kernel_data[w] = 0;
    }

    int idx = 0;
    for (int r = 0; r < KERNEL_SIZE; r++) {
        for (int c = 0; c < KERNEL_SIZE; c++) {
            int word_idx = idx / 4;
            int offset = (idx % 4) * 16;
            packed_kernel_data[word_idx] |= ((uint64_t)float_to_fixed88(kernel_data[r][c]) << offset);
            idx++;
        }
    }

    printf("Kernel size: %d x %d\n", KERNEL_SIZE, KERNEL_SIZE);
    printf("input tile size: %d x %d\n", INPUT_TILE_SIZE, INPUT_TILE_SIZE);

    int pad = 0;
    if (KERNEL_SIZE == 1) {
        pad = 0; // 1x1 kernel
    } else if (KERNEL_SIZE == 3) {
        pad = 1; // 3x3 kernel
    } else if (KERNEL_SIZE == 5) {
        pad = 2; // 5x5 kernel
    } else {
        fprintf(stderr, "Unsupported KERNEL_SIZE. Must be 1, 3, or 5.\n");
        exit(1);
    }

    printf("Load kernel...\n");
    uint64_t success = doLoadKernel((uint64_t)&packed_kernel_data[0], pad);
    printf("RoCC instruction returned: %lu\n", success);
    

    // input tile handling
    int tileType = 0;

    //for (int i = 0; i < INPUT_SIZE; i++) {
    //    for (int j = 0; j < INPUT_SIZE; j++) {
    //        printf("%2.0f ", input[i * INPUT_SIZE + j]);
    //    }
    //    printf("\n");
    //}
    printf("Input address: 0x%lx\n", (uintptr_t)&input[0]);

    int rowStart = 0;
    int rowEnd = 0;
    int colStart = 0;
    int colEnd = 0;
    int outRowStart = 0;
    int outRowEnd = 0;
    int outColStart = 0;
    int outColEnd = 0;
    int count = 0;


    asm volatile("fence" ::: "memory");
    uint64_t result; 

    for (int i = 0; i < INPUT_SIZE/OUTPUT_TILE_SIZE; i++) {
        for (int j = 0; j < INPUT_SIZE/OUTPUT_TILE_SIZE; j++) {
            if (i == 0 && j == 0) {
                tileType = TOP_LEFT; // Top-left corner

                rowStart = 0;
                rowEnd = INPUT_TILE_SIZE - 1;
                colStart = 0;
                colEnd = INPUT_TILE_SIZE - 1;

                outRowStart = 0;
                outColStart = 0;
            } else if (i == 0 && j == INPUT_SIZE/OUTPUT_TILE_SIZE - 1) {
                tileType = TOP_RIGHT; // Top-right corner

                rowStart = 0;
                rowEnd = INPUT_TILE_SIZE - 1;
                colStart = INPUT_SIZE - INPUT_TILE_SIZE;
                colEnd = INPUT_SIZE - 1;

                outRowStart = 0;
                outColStart = OUTPUT_SIZE - OUTPUT_TILE_SIZE;
            } else if (i == INPUT_SIZE/OUTPUT_TILE_SIZE - 1 && j == 0) {
                tileType = BOTTOM_LEFT; // Bottom-left corner

                rowStart = INPUT_SIZE - INPUT_TILE_SIZE;
                rowEnd = INPUT_SIZE - 1;
                colStart = 0;
                colEnd = INPUT_TILE_SIZE - 1;

                outRowStart = OUTPUT_SIZE - OUTPUT_TILE_SIZE;
                outColStart = 0;
            } else if (i == INPUT_SIZE/OUTPUT_TILE_SIZE - 1 && j == INPUT_SIZE/OUTPUT_TILE_SIZE - 1) {
                tileType = BOTTOM_RIGHT; // Bottom-right corner

                rowStart = INPUT_SIZE - INPUT_TILE_SIZE;
                rowEnd = INPUT_SIZE - 1;
                colStart = INPUT_SIZE - INPUT_TILE_SIZE;
                colEnd = INPUT_SIZE - 1;

                outRowStart = OUTPUT_SIZE - OUTPUT_TILE_SIZE;
                outColStart = OUTPUT_SIZE - OUTPUT_TILE_SIZE;
            } else if (i == 0) {
                tileType = TOP; // Top edge

                rowStart = 0;
                rowEnd = INPUT_TILE_SIZE - 1;
                colStart = j * OUTPUT_TILE_SIZE - pad;
                colEnd = colStart + INPUT_TILE_SIZE - 1;

                outRowStart = 0;
                outColStart = j * OUTPUT_TILE_SIZE;
            } else if (j == 0) {
                tileType = LEFT; // Left edge

                rowStart = i * OUTPUT_TILE_SIZE - pad;
                rowEnd = rowStart + INPUT_TILE_SIZE - 1;
                colStart = 0;
                colEnd = INPUT_TILE_SIZE - 1;

                outRowStart = i * OUTPUT_TILE_SIZE;
                outColStart = 0;
            } else if (i == INPUT_SIZE/OUTPUT_TILE_SIZE - 1) {
                tileType = BOTTOM; // Bottom edge
                rowStart = INPUT_SIZE - INPUT_TILE_SIZE;
                rowEnd = INPUT_SIZE - 1;
                colStart = j * OUTPUT_TILE_SIZE - pad;
                colEnd = colStart + INPUT_TILE_SIZE - 1;

                outRowStart = OUTPUT_SIZE - OUTPUT_TILE_SIZE;
                outColStart = j * OUTPUT_TILE_SIZE;
            } else if (j == INPUT_SIZE/OUTPUT_TILE_SIZE - 1) {
                tileType = RIGHT; // Right edge
                rowStart = i * OUTPUT_TILE_SIZE - pad;
                rowEnd = rowStart + INPUT_TILE_SIZE - 1;
                colStart = INPUT_SIZE - INPUT_TILE_SIZE;
                colEnd = INPUT_SIZE - 1;

                outRowStart = i * OUTPUT_TILE_SIZE;
                outColStart = OUTPUT_SIZE - OUTPUT_TILE_SIZE;
            } else {
                tileType = CENTER; // Center tile
                rowStart = i * OUTPUT_TILE_SIZE - pad;
                rowEnd = rowStart + INPUT_TILE_SIZE - 1;
                colStart = j * OUTPUT_TILE_SIZE - pad;
                colEnd = colStart + INPUT_TILE_SIZE - 1;

                outRowStart = i * OUTPUT_TILE_SIZE;
                outColStart = j * OUTPUT_TILE_SIZE;
            }
            printf("Tile type: %d, i: %d, j: %d, rowStart: %d, rowEnd: %d, colStart: %d, colEnd: %d\n", tileType, i, j, rowStart, rowEnd, colStart, colEnd);
            count = 0;
            int tx = 0, ty = 0;
            for (int x = rowStart; x <= rowEnd; x++) {
                ty = 0;
                for (int y = colStart; y <= colEnd; y++) {
                    //printf("x: %d, y: %d, tx: %d, ty: %d, flat: %d, tflat: %d\n", x, y, tx, ty, x * INPUT_SIZE + y, tx * INPUT_TILE_SIZE + ty);
                    input_tile[tx * INPUT_TILE_SIZE + ty] = input[x * INPUT_SIZE + y];
                    ty++;
                }
                tx++;
            }
            //for (int a = 0 ; a < INPUT_TILE_SIZE; a++) {
            //    for (int b = 0; b < INPUT_TILE_SIZE; b++) {
            //        printf("%2.0f ", input_tile[a * INPUT_TILE_SIZE + b]);
            //    }
            //    printf("\n");
            //}
            for (int w = 0; w < PACKED_INPUT_TILE_LEN; w++) {
                input_tile_packed[w] = 0;
            }
            idx = 0;
            for (int r = 0; r < INPUT_TILE_SIZE; r++) {
                for (int c = 0; c < INPUT_TILE_SIZE; c++) {
                    int word_idx = idx / 4;
                    int offset = (idx % 4) * 16;
                    //printf("r: %d, c: %d, val: %f\n",r,c, input_tile[r*INPUT_TILE_SIZE+c]);
                    input_tile_packed[word_idx] |= ((uint64_t)float_to_fixed88(input_tile[r*INPUT_TILE_SIZE+c]) << offset);
                    idx++;
                }
            }
            //printf("Packed input data:\n");
            //for (int i = 0; i < PACKED_INPUT_TILE_LEN; i++) {
            //    printf("Word%d at addr 0x%lx: 0x%016lx\n", i, (uintptr_t)&input_tile_packed[i], input_tile_packed[i]);
            //}
            //printf("Load input...\n");
            success = InputLoad((uint64_t)&input_tile_packed[0], 0); // 0 = address of first element, can be adjusted as needed
            //printf("RoCC instruction returned: %lu\n", success);
            //printf("Input Loaded!\n");
            //printf("Starting computation... with tile type = %d\n",tileType);
            //printf("Output address: 0x%lx\n", (uintptr_t)&output_tile_packed[0]);
            result = doCompute((uint64_t)&output_tile_packed[0], tileType); // 0 = tileType, can be adjusted as needed

            // Write output tile to output array
            
            for (int i = 0; i < PACKED_OUTPUT_TILE_LEN; i++) {
                uint16_t v0 = (output_tile_packed[i] >> 0) & 0xFFFF;
                uint16_t v1 = (output_tile_packed[i] >> 16) & 0xFFFF;
                uint16_t v2 = (output_tile_packed[i] >> 32) & 0xFFFF;
                uint16_t v3 = (output_tile_packed[i] >> 48) & 0xFFFF;

                output_tile_f88[i * 4 + 0] = v0;
                output_tile_f88[i * 4 + 1] = v1;
                output_tile_f88[i * 4 + 2] = v2;
                output_tile_f88[i * 4 + 3] = v3;

                output_tile[i * 4 + 0] = fixed88_to_float(v0);
                output_tile[i * 4 + 1] = fixed88_to_float(v1);
                output_tile[i * 4 + 2] = fixed88_to_float(v2);
                output_tile[i * 4 + 3] = fixed88_to_float(v3);
            }

            for (int tx = 0; tx < OUTPUT_TILE_SIZE; tx++) {
                for (int ty = 0; ty < OUTPUT_TILE_SIZE; ty++) {
                    output[(outRowStart + tx) * OUTPUT_SIZE + (outColStart + ty)] = output_tile[tx * OUTPUT_TILE_SIZE + ty];
                    //printf("0x%04x ", output_tile_f88[tx * OUTPUT_TILE_SIZE + ty]);
                }
                //printf("\n");
            }
            printf("Overflowed indices: ");
            int ovflx = 0, ovfly = 0;
            for (int i = 0; i < OUTPUT_SIZE * OUTPUT_SIZE; i++) {
                if ((result >> i) & 1) {
                    ovflx = i / OUTPUT_TILE_SIZE;
                    ovfly = i % OUTPUT_TILE_SIZE;
                    printf("flat: %d, x: %d, y: %d || ", i, ovflx, ovfly);
                }
            }
            printf("\n");
            
        }
    }

    for (int i = 0; i < OUTPUT_SIZE; i++) {
        for (int j = 0; j < OUTPUT_SIZE; j++) {
            int16_t fx = (int16_t)(output[i * OUTPUT_SIZE + j] * 256.0f);
            int integer = fx >> 8;
            int fraction = fx & 0xFF;
            printf("%d.%02d ", integer, fraction);
        }
    printf("\n");
    }

    return 0;
}