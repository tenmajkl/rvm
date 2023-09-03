#include <stdio.h>
#include <stdint.h>

void generate(FILE* f, uint8_t code[][4], int size)
{
    for (int i = 0; i < size; i++) {
        fwrite(&code[i], sizeof(uint8_t), 4, f);
    }
}

int main(void)
{
    FILE* f = fopen("test.rbf", "wb");
    uint8_t code[100][4] = {
        {1, 0, 0, 5},
        {1, 1, 0, 1},
        {3, 0, 1, 0},
        {13, 0, 0, 0},
        {10, 0, 0, 6},
        {9, 0, 2, 0},
        {0, 0, 0, 0},
    };
    generate(f, code, 7);
    fclose(f);
    return 0;
}
