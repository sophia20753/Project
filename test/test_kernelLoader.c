#include <stdint.h>
#include <stdio.h>
#include "rocc.h"

#define KERNEL_SIZE 5
#define KERNEL_LEN (KERNEL_SIZE * KERNEL_SIZE)
#define CUSTOM_OPCODE 0
#define FUNCT7_DOLK 0x03

static inline void load_kernel(uint64_t ptr, uint64_t k_size) {
    // rd = 0, funct3 = 0b011, funct7 = 0b0000000
    // size_mode in rs1 (0=1x1, 1=3x3, 2=5x5)
    // ptr in rs2 = address of first element
    ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, k_size, ptr, FUNCT7_DOLK);
}

int main() {
    // Example 5x5 kernel (flattened row-wise)
    int32_t kernel[KERNEL_LEN] = {
        1,  0, -1,  0,  1,
        2,  0, -2,  0,  2,
        3,  0, -3,  0,  3,
        2,  0, -2,  0,  2,
        1,  0, -1,  0,  1
    };

    printf("Sending kernel pointer to accelerator...\n");
    load_kernel((uint64_t)&kernel[0], 2); // 2 = 5x5 kernel
    printf("Instruction sent!\n");
    return 0;
}