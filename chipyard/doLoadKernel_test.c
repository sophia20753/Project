#include <stdint.h>
#include <stdio.h>
#include "rocc.h"
#include "unistd.h"

#define CUSTOM_OPCODE 0
#define FUNCT7_DOLOADKERNEL 0x02 // 0b0000010
#define KERNEL_SIZE 3

// Create a wrapper to issue your custom instruction
static inline uint64_t doLoadKernel(uint64_t kernel_ptr, uint64_t kernel_size) {
    uint64_t result;
    // ROCC_INSTRUCTION_DSS(opcode, rd, rs1, rs2, funct7)
    // rs1 = kernel address
    // rs2 = kernel size
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, kernel_ptr, kernel_size, FUNCT7_DOLOADKERNEL);
    return result;
}

// Kernel definition based on size
#if KERNEL_SIZE == 1
static uint16_t kernel_data[KERNEL_SIZE][KERNEL_SIZE] = {
    {1}
};
#elif KERNEL_SIZE == 3
static uint16_t kernel_data[KERNEL_SIZE][KERNEL_SIZE] = {
    {1, 2, 3},
    {4, 5, 6},
    {7, 8, 9}
};
#elif KERNEL_SIZE == 5
static uint16_t kernel_data[KERNEL_SIZE][KERNEL_SIZE] = {
    {1, 2, 3, 4, 5},
    {1, 2, 3, 4, 5},
    {1, 2, 3, 4, 5},
    {1, 2, 3, 4, 5},
    {1, 2, 3, 4, 5}
};
#else
#error "Unsupported KERNEL_SIZE. Must be 1, 3, or 5."
#endif

int main() {
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
            packed_kernel_data[word_idx] |= ((uint64_t)kernel_data[r][c] << offset);
            idx++;
        }
    }

    printf("Packed kernel data:\n");
    for (int i = 0; i < num_words; i++) {
        printf("Word%d at addr 0x%lx: 0x%016lx\n", i, (uintptr_t)&packed_kernel_data[i], packed_kernel_data[i]);
    }

    uint64_t success = doLoadKernel((uint64_t)&packed_kernel_data[0], KERNEL_SIZE);
    printf("RoCC instruction returned: %lu\n", success);

    return 0;
}