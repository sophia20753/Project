#include <stdio.h>


void conv(float input[32][32],
    float kernel[5][5],
    float output[32][32])
{
    //calc output feature map
    for (int ro=0; ro < 32; ro++) {
        for (int co=0; co < 32; co++) {
            // sum kernel
            for (int i=0; i < 5; i++) {
                for (int j=0; j < 5; j++) {
                    // this depends on the current element/tile to decide if MAC is needed so that same size padding is maintained
                    output[ro][co] += kernel[i][j] * input[ro+i][co+j];
                }
            }
        }
    }

}