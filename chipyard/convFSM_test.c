#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "rocc.h"

#define KERNEL_SIZE 1
#define INPUT_SIZE 8
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
#define FUNCT7_KERNELLOAD 0x02
#define FUNCT7_DOCOMPUTE 0x03
#define FUNCT7_KERNELSIZE 0x04

static inline void doprint() {
    // rd = 0, funct3 = 0b011, funct7 = 0b0000000
    // size_mode in rs1 (0=1x1, 1=3x3, 2=5x5)
    // ptr in rs2 = address of first element
    //ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, k_size, ptr, FUNCT7_DOLK);
    ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, 0, 0, FUNCT7_DOLK);
    return;
}

static inline void SetKernelSize(uint64_t size) {
    ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, size, 0, FUNCT7_KERNELSIZE);
    return;
}

static inline void KernelLoad(uint64_t ptr, uint64_t addr) {
    ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, ptr, addr, FUNCT7_KERNELLOAD);
    return;
}

static inline void InputLoad(uint64_t ptr, uint64_t addr) {
    ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, ptr, addr, FUNCT7_INPUTLOAD);
    return;
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

int main() {
    
    /*
    // Example 5x5 kernel (flattened row-wise)
    float kernel[KERNEL_LEN] = {
        1.0,  5.0, -1.0,  0.5,  1.0,
        2.0,  0.5, -2.0,  0.5,  2.0,
        3.0,  0.5, -3.0,  0.5,  3.0,
        2.0,  0.5, -2.0,  0.5,  2.0,
        1.0,  0.5, -1.0,  0.5,  1.0
    };

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
    
    */
    /*
    float kernel[KERNEL_LEN] = {
        1.0,  5.0, -1.0,
        2.0,  0.5, -2.0,
        3.0,  0.5, -3.0
    };

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
    };
    */

    float kernel[KERNEL_LEN] = {1.5};
    
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
    
    
    //uint16_t kernel_fx[KERNEL_LEN];
    uint64_t kernel_packed[PACKED_KERNEL_LEN];
    uint64_t input_packed[PACKED_INPUT_LEN];
     // 1 packet = 4 values
    uint64_t output_packed[PACKED_OUTPUT_LEN];
    uint16_t* unpacked_view = (uint16_t*)output_packed;
    // Convert floats to fixed-point 8.8
    //for (int i = 0; i < KERNEL_LEN; i++) {
    //    kernel_fx[i] = float_to_fixed88(kernel[i]);
    //    printf("kernel[%d] = %f -> fixed 8.8 = %4x\n", i, kernel[i], kernel_fx[i]);
    //}

    // Pack four 16-bit values into each 64-bit word

    printf("Kernel address: 0x%lx\n", (uintptr_t)kernel_packed);

    for (int i = 0; i < PACKED_KERNEL_LEN; i++) {
        kernel_packed[i] = 0;
        kernel_packed[i] |= (uint64_t)float_to_fixed88(kernel[4*i]) << 48;
        kernel_packed[i] |= (uint64_t)float_to_fixed88(kernel[4*i + 1]) << 32;
        kernel_packed[i] |= (uint64_t)float_to_fixed88(kernel[4*i + 2]) << 16;
        kernel_packed[i] |= (uint64_t)float_to_fixed88(kernel[4*i + 3]); // First 16 bits
    }

    printf("Input address: 0x%lx\n", (uintptr_t)input_packed);

    for (int i = 0; i < PACKED_INPUT_LEN; i++) {
        input_packed[i] = 0;
        input_packed[i] |= (uint64_t)float_to_fixed88(input[4*i]) << 48;
        input_packed[i] |= (uint64_t)float_to_fixed88(input[4*i + 1]) << 32;
        input_packed[i] |= (uint64_t)float_to_fixed88(input[4*i + 2]) << 16;
        input_packed[i] |= (uint64_t)float_to_fixed88(input[4*i + 3]); // First 16 bits
    }
    

    asm volatile("fence" ::: "memory");
    uint64_t result; 

    //SetKernelSize(2); // 5x5 kernel
    //SetKernelSize(1); // 3x3 kernel
    SetKernelSize(0); // 1x1 kernel

    for (int i = 0; i < PACKED_KERNEL_LEN; i++) {
        KernelLoad((uint64_t)&kernel_packed[i], (uint64_t)i); // 2 = 5x5 kernel
        printf("Sending kernel %d, %d, %d, %d: 0x%0.16lx\n", 4*i,4*i+1,4*i+2,4*i+3, kernel_packed[i]);
    }
    printf("Kernel Loaded!\n");

    for (int i = 0; i < PACKED_INPUT_LEN; i++) {
        InputLoad((uint64_t)&input_packed[i], (uint64_t)i); // 2 = 5x5 kernel
        printf("Sending input %d, %d, %d, %d: 0x%0.16lx\n", 4*i,4*i+1,4*i+2,4*i+3, input_packed[i]);
    }
    printf("Kernel Loaded!\n");

    doprint();

    int tileType = 3; // Example tile type, can be adjusted as needed
    printf("Starting computation... with tile type = %d\n",tileType);
    printf("Output address: 0x%lx\n", (uintptr_t)&output_packed[0]);
    result = doCompute((uint64_t)&output_packed[0], tileType); // 0 = tileType, can be adjusted as needed
    
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