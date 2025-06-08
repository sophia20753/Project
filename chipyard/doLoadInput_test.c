#include <stdint.h>
#include <stdio.h>
#include "rocc.h"
#include "unistd.h"

#define CUSTOM_OPCODE 0
#define FUNCT7_DOLOADINPUT 0x01 // 0b0000001
#define INPUT_SIZE 32
#define TILE_NUM 1

// Create a wrapper to issue your custom instruction
static inline uint64_t doLoadInput(uint64_t input_ptr, uint64_t tile_num) {
    uint64_t result;
    // ROCC_INSTRUCTION_DSS(opcode, rd, rs1, rs2, funct7)
    // rs1 = kernel address
    // rs2 = kernel size
    ROCC_INSTRUCTION_DSS(CUSTOM_OPCODE, result, input_ptr, tile_num, FUNCT7_DOLOADINPUT);
    return result;
}


static uint16_t input_data[INPUT_SIZE][INPUT_SIZE] = {0};

void generate_input_data() {
    // Initialize input data to some deterministic values
    // This example fills input_data with row * 32 + col
    for (int r = 0; r < INPUT_SIZE; r++) {
        for (int c = 0; c < INPUT_SIZE; c++) {
            input_data[r][c] = r * INPUT_SIZE + c;
        }
    }

    // printf("Input matrix (%dx%d):\n", INPUT_SIZE, INPUT_SIZE);
    // for (int r = 0; r < INPUT_SIZE; r++) {
    //     for (int c = 0; c < INPUT_SIZE; c++) {
    //         printf("%4d ", input_data[r][c]);
    //     }
    //     printf("\n");
    // }
    // printf("\n");
}

int main() {
    generate_input_data();

    int num_elements = INPUT_SIZE * INPUT_SIZE;
    int num_words = (num_elements + 4) / 4;

    static uint64_t packed_input_data[(INPUT_SIZE * INPUT_SIZE + 3) / 4] = {0};

    int idx = 0;
    for (int r = 0; r < INPUT_SIZE; r++) {
        for (int c = 0; c < INPUT_SIZE; c++) {
            int word_idx = idx / 4;
            int offset = (idx % 4) * 16;
            packed_input_data[word_idx] |= ((uint64_t)input_data[r][c] << offset);
            idx++;
        }
    }

    for (int tiles = 0; tiles < 1; tiles++) {
        printf("Packed input data (TILE_NUM = %d):\n", tiles);
        int max_elements = 64; // each 8x8 tile is 64 elements
        int max_words = (max_elements + 3) / 4;
        for (int i = 0; i < max_words; i++) {
            printf("Word %2d at addr 0x%lx: 0x%016lx\n", i,
                (uintptr_t)&packed_input_data[i],
                packed_input_data[i]);
        }
    }

    printf("Sending RoCC instruction\n");
    

    uint64_t success = doLoadInput((uint64_t)&packed_input_data[0], TILE_NUM);
    printf("RoCC instruction returned: %lu\n", success);

    return 0;
}