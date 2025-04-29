#include <stdio.h>


void conv(float input[32][32],
    float kernel[5][5],
    float output[32][32])
{
    // same-padding
    int p = 2;
    float padded_input[36][36];

    for (int r=0; r < 36; r++) {
        for (int c=0; c < 36; c++) {
            padded_input[r][c] = 0;
        }
    }

    for (int r=0; r < 32; r++) {
        for (int c=0; c < 32; c++) {
            padded_input[r+p][c+p] = input[r][c];
        }
    }

    
    //calc output feature map
    for (int ro=0; ro < 32; ro++) {
        for (int co=0; co < 32; co++) {
            // sum kernel
            for (int i=0; i < 5; i++) {
                for (int j=0; j < 5; j++) {
                    output[ro][co] += kernel[i][j] * padded_input[ro+i][co+j];
                }
            }
        }
    }

}