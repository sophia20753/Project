#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "rocc.h"

#define KERNEL_SIZE 5
#define INPUT_SIZE 12
#define OUTPUT_SIZE 8
#define INPUT_LEN (INPUT_SIZE * INPUT_SIZE)
#define KERNEL_LEN (KERNEL_SIZE * KERNEL_SIZE)
#define OUTPUT_LEN (OUTPUT_SIZE * OUTPUT_SIZE) // Not used in this test, but defined for completeness
#define PACKED_KERNEL_LEN ((KERNEL_LEN + 3) / 4) // 4 values per 64-bit word
#define PACKED_INPUT_LEN ((INPUT_LEN + 3) / 4) // 4 values per 64-bit word
#define PACKED_OUTPUT_LEN ((OUTPUT_LEN + 3) / 4) // 4 values per 64-bit word
#define CUSTOM_OPCODE 0
#define FUNCT7_DOLK 0x00
#define FUNCT7_INPUTLOAD 0x01
#define FUNCT7_DOLOADKERNEL 0x02 // 0b0000010
#define FUNCT7_DOCOMPUTE 0x03
#define FUNCT7_KERNELSIZE 0x04


static inline uint64_t rdcycle() {

	uint64_t cycles;
	asm volatile ("rdcycle %0" : "=r" (cycles));
	return cycles;
}

static inline void doprint() {
    // rd = 0, funct3 = 0b011, funct7 = 0b0000000
    // size_mode in rs1 (0=1x1, 1=3x3, 2=5x5)
    // ptr in rs2 = address of first element
    //ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, k_size, ptr, FUNCT7_DOLK);
    ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, 0, 0, FUNCT7_DOLK);
    return;
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
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, input_ptr, 0, FUNCT7_DOLOADKERNEL);
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
    
    /*
    // Example 5x5 kernel (flattened row-wise)
    float kernel[KERNEL_LEN] = {
        1.0,  5.0, -1.0,  0.5,  1.0,
        2.0,  0.5, -2.0,  0.5,  2.0,
        3.0,  0.5, -3.0,  0.5,  3.0,
        2.0,  0.5, -2.0,  0.5,  2.0,
        1.0,  0.5, -1.0,  0.5,  1.0
    };*/

    float input[INPUT_LEN] = {
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,2,3,4,5,6,7,8,9,9,
        1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1
        };
    
    
    /*
    float kernel[KERNEL_LEN] = {
        1.0,  5.0, -1.0,
        2.0,  0.5, -2.0,
        3.0,  0.5, -3.0
    };*/
    /*
    float input[INPUT_LEN] = {
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1, 
        1,2,3,4,5,6,7,8,9,1,
        1,2,3,4,5,6,7,8,9,1     
    };*/
    

    //float kernel[KERNEL_LEN] = {1.5};
    /*
    float input[INPUT_LEN] = {
        1,2,3,4,5,6,7,8,
        1,2,3,4,5,6,7,8,
        1,2,3,4,5,6,7,8,
        1,2,3,4,5,6,7,8,
        1,2,3,4,5,6,7,8,
        1,2,3,4,5,6,7,8,
        1,2,3,4,5,6,7,8,
        1,2,3,4,5,6,7,8
    };
    */
    
    int num_elements = KERNEL_SIZE * KERNEL_SIZE;
    int num_words = (num_elements + 4) / 4;

    uint64_t packed_kernel_data[num_words];
    for (int w = 0; w < num_words; w++) {
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

    printf("Packed kernel data:\n");
    for (int i = 0; i < num_words; i++) {
        printf("Word%d at addr 0x%lx: 0x%016lx\n", i, (uintptr_t)&packed_kernel_data[i], packed_kernel_data[i]);
    }
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
    uint64_t input_packed[PACKED_INPUT_LEN];
    uint64_t output_packed[PACKED_OUTPUT_LEN];
    uint16_t* unpacked_view = (uint16_t*)output_packed;

    printf("Input address: 0x%lx\n", (uintptr_t)input_packed);

    for (int w = 0; w < PACKED_INPUT_LEN; w++) {
        input_packed[w] = 0;
    }

    idx = 0;
    for (int r = 0; r < INPUT_SIZE; r++) {
        for (int c = 0; c < INPUT_SIZE; c++) {
            int word_idx = idx / 4;
            int offset = (idx % 4) * 16;
            input_packed[word_idx] |= ((uint64_t)float_to_fixed88(input[r*INPUT_SIZE+c]) << offset);
            idx++;
        }
    }
    printf("Packed input data:\n");
    for (int i = 0; i < PACKED_INPUT_LEN; i++) {
        printf("Word%d at addr 0x%lx: 0x%016lx\n", i, (uintptr_t)&input_packed[i], input_packed[i]);
    }

    printf("Start\n");
    int start = rdcycle();
    printf("Load kernel...\n");
    uint64_t success = doLoadKernel((uint64_t)&packed_kernel_data[0], pad);
    printf("RoCC instruction returned: %lu\n", success);
    

    asm volatile("fence" ::: "memory");
    uint64_t result; 


    printf("Load input...\n");
    success = InputLoad((uint64_t)&input_packed[0], 0); // 0 = address of first element, can be adjusted as needed
    printf("RoCC instruction returned: %lu\n", success);
    printf("Input Loaded!\n");

    doprint();

    int tileType = 3; // Example tile type, can be adjusted as needed
    printf("Starting computation... with tile type = %d\n",tileType);
    printf("Output address: 0x%lx\n", (uintptr_t)&output_packed[0]);
    result = doCompute((uint64_t)&output_packed[0], tileType); // 0 = tileType, can be adjusted as needed
    int end = rdcycle();
    printf("Execution took %lu cycles\n",end-start);
    printf("Convolution tile done\n");


    for (int i = 0; i < PACKED_OUTPUT_LEN; i++) {
        uint16_t v0 = (output_packed[i] >> 0) & 0xFFFF;
        uint16_t v1 = (output_packed[i] >> 16) & 0xFFFF;
        uint16_t v2 = (output_packed[i] >> 32) & 0xFFFF;
        uint16_t v3 = (output_packed[i] >> 48) & 0xFFFF;

        printf("At addr 0x%lx, raw 16-bit values: 0x%04x 0x%04x 0x%04x 0x%04x\n", (uintptr_t)&output_packed[i], v0, v1, v2, v3);
    }

    if (result == 1) {
        printf("Accelerator done loading kernel.\n");
    } else {
        printf("Accelerator did not return success.\n");
    }
    return 0;
}