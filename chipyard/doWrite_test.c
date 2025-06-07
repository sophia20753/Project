#include <stdint.h>
#include <stdio.h>
#include "rocc.h"

#define CUSTOM_OPCODE 0
#define FUNCT7_DOLOADMATRIX 0x01 // 0b0000001
#define FUNCT3_DOLOADMATRIX 0x3  // 0b011

// Create a wrapper to issue your custom instruction
static inline uint64_t doLoadMatrix(uint64_t num_elements, uint64_t matrix_ptr) {
    uint64_t result;
    // ROCC_INSTRUCTION_DSS(opcode, rd, rs1, rs2, funct7)
    // rs1 = is_output (0 = input matrix)
    // testbench rs1 = num_elements to write
    // rs2 = matrix_ptr
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, num_elements, matrix_ptr, FUNCT7_DOLOADMATRIX);
    return result;
}

int main() {
    // A tiny 1x1 test "matrix" (a single 64-bit word)
    static uint64_t matrix_data[4] = {0, 0, 0, 0};

    printf("Matrix address: 0x%lx\n", (uintptr_t)&matrix_data[0]);

    asm volatile("fence" ::: "memory"); // Ensure memory is visible to RoCC

    for (int i = 0; i < 4; ++i) {
        printf("matrix_data[%d] = %u (0x%04x)\n", i, matrix_data[i], matrix_data[i]);
    }

    // Call the accelerator to load matrix
    uint64_t result = doLoadMatrix(4, (uint64_t)&matrix_data[0]);  // 0 = input matrix

    printf("Instruction sent. Accelerator returned: 0x%lx\n", result);
    
    for (int i = 0; i < 4; ++i) {
        printf("matrix_data[%d] = %u (0x%04x)\n", i, matrix_data[i], matrix_data[i]);
    }

    if (result == 1) {
        printf("Accelerator acknowledged load.\n");
    } else {
        printf("Accelerator did not return expected result.\n");
    }

    return 0;
}
