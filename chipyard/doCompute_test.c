#include <stdint.h>
#include <stdio.h>
#include "rocc.h"
#include "unistd.h"

#define CUSTOM_OPCODE 0
#define FUNCT7_DOLOADMATRIX 0x01 // 0b0000001
#define FUNCT7_DOCOMPUTE 0x03 // 0b0000011
#define FUNCT3_DOLOADMATRIX 0x3  // 0b011

static inline uint64_t doCompute(uint64_t matrix_ptr, uint64_t input_type) {
    uint64_t result;

    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, matrix_ptr, input_type, FUNCT7_DOCOMPUTE);
    return result;
}

int main() {
    static uint64_t matrix_data[1] = {0}; // 1 packet = 4 values
    uint16_t* unpacked_view = (uint16_t*)matrix_data;

    printf("Matrix address: 0x%lx\n", (uintptr_t)&matrix_data[0]);

    asm volatile("fence" ::: "memory");

    uint64_t overflow_result = doCompute((uint64_t)&matrix_data[0], 4);

    __sync_synchronize();
    asm volatile("fence iorw, iorw" ::: "memory");

    printf("Instruction sent. Accelerator returned: 0x%lx\n",overflow_result);

    printf("After:\n");
    for (int i = 0; i < 1; ++i) {
        uint16_t v0 = (matrix_data[i] >> 0) & 0xFFFF;
        uint16_t v1 = (matrix_data[i] >> 16) & 0xFFFF;
        uint16_t v2 = (matrix_data[i] >> 32) & 0xFFFF;
        uint16_t v3 = (matrix_data[i] >> 48) & 0xFFFF;

        printf("At addr 0x%lx, raw 16-bit values: 0x%04x 0x%04x 0x%04x 0x%04x\n", (uintptr_t)&matrix_data[i], v0, v1, v2, v3);
    }

    _exit(0);
}