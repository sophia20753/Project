#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "rocc.h"
#include "unistd.h"

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
    static uint64_t matrix_data[4] = {0, 0, 0, 0}; // 4 packed words = 16 values max
    uint16_t* unpacked_view = (uint16_t*)matrix_data;

    printf("Matrix address: 0x%lx\n", (uintptr_t)&matrix_data[0]);

    asm volatile("fence" ::: "memory");

    printf("Before:\n");
    for (int i = 0; i < 16; ++i) {
        printf("unpacked_view[%d] = %u (0x%04x)\n", i, unpacked_view[i], unpacked_view[i]);
    }

    uint64_t result = doLoadMatrix(16, (uint64_t)&matrix_data[0]);

    printf("Instruction sent. Accelerator returned: 0x%lx\n", result);

    printf("After:\n");
    for (int i = 0; i < 16; ++i) {
        printf("unpacked_view[%d] = %u (0x%04x)\n", i, unpacked_view[i], unpacked_view[i]);
    }

    if (result == 1) {
        printf("Accelerator acknowledged load.\n");
    } else {
        printf("Accelerator did not return expected result.\n");
    }

    _exit(0);
}