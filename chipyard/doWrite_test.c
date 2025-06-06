#include <stdint.h>
#include <stdio.h>
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

    // printf("Before:\n");
    // for (int i = 0; i < 4; ++i) {
    //     printf("unpacked_view[%d] = %u (0x%04x)\n", i, matrix_data[i], matrix_data[i]);
    // }

    uint64_t result = doLoadMatrix(4, (uint64_t)&matrix_data[0]);

    __sync_synchronize();
    asm volatile("fence iorw, iorw" ::: "memory");

    printf("Instruction sent. Accelerator returned: 0x%lx\n", result);

    printf("After:\n");
    for (int i = 0; i < 4; ++i) {
        uint16_t v0 = (matrix_data[i] >> 0) & 0xFFFF;
        uint16_t v1 = (matrix_data[i] >> 16) & 0xFFFF;
        uint16_t v2 = (matrix_data[i] >> 32) & 0xFFFF;
        uint16_t v3 = (matrix_data[i] >> 48) & 0xFFFF;

        printf("At addr 0x%lx, raw 16-bit values: 0x%04x 0x%04x 0x%04x 0x%04x\n", (uintptr_t)&matrix_data[i], v0, v1, v2, v3);
    }

    if (result == 1) {
        printf("Accelerator acknowledged load.\n");
    } else {
        printf("Accelerator did not return expected result.\n");
    }

    _exit(0);
}