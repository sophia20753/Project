#include <stdint.h>
#include <stdio.h>
#include "rocc.h"

#define KERNEL_SIZE 5
#define KERNEL_LEN (KERNEL_SIZE * KERNEL_SIZE)
#define CUSTOM_OPCODE 0
#define FUNCT7_DOLK 0x00
#define FUNCT7_TESTLOAD 0x02

static inline uint64_t load_kernel(uint64_t ptr, uint64_t k_size) {
    // rd = 0, funct3 = 0b011, funct7 = 0b0000000
    // size_mode in rs1 (0=1x1, 1=3x3, 2=5x5)
    // ptr in rs2 = address of first element
    //ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, k_size, ptr, FUNCT7_DOLK);
    uint64_t result;
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, k_size, ptr, FUNCT7_DOLK);
    return result;
}

static inline uint64_t test_doLoad(uint64_t ptr, uint64_t addr) {
    // rd = 0, funct3 = 0b011, funct7 = 0b0000010
    // size_mode in rs1 (0=1x1, 1=3x3, 2=5x5)
    // ptr in rs2 = address of first element
    //ROCC_INSTRUCTION_SS(CUSTOM_OPCODE, k_size, ptr, FUNCT7_DOLK);
    uint64_t result;
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, ptr, addr,FUNCT7_TESTLOAD);
    return result;
}

int main() {
    // Example 5x5 kernel (flattened row-wise)
    static int32_t kernel[KERNEL_LEN] = {
        1,  0, -1,  0,  1,
        2,  0, -2,  0,  2,
        3,  0, -3,  0,  3,
        2,  0, -2,  0,  2,
        1,  0, -1,  0,  1
    };

    printf("Kernel address: 0x%lx\n", (uintptr_t)kernel);
    printf("Sending kernel pointer to accelerator...\n");
    printf("size: %d",sizeof(kernel[0]));
    asm volatile("fence" ::: "memory");
    uint64_t result = test_doLoad((uint64_t)&kernel[0], (uint64_t) 2); // 2 = 5x5 kernel
    //uint64_t result = load_kernel((uint64_t)&kernel[0], 2); // 2 = 5x5 kernel
    printf("Instruction sent!\n");

    if (result == 1) {
        printf("Accelerator done loading kernel.\n");
    } else {
        printf("Accelerator did not return success.\n");
    }
    return 0;
}