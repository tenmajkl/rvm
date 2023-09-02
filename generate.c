#include <stdio.h>
#include <stdint.h>

int main(void)
{
    FILE* f = fopen("test.rbf", "wb");
    uint8_t parek[4] = {1, 1, 0, 1};
    fwrite(&parek, sizeof(uint8_t), 4, f);
    uint8_t parekk[4] = {1, 2, 0, 1};
    fwrite(&parekk, sizeof(uint8_t), 4, f);
    uint8_t parekkk[4] = {2, 1, 2, 1};
    fwrite(&parekkk, sizeof(uint8_t), 4, f);
    uint8_t parekkkk[4] = {9, 0, 1, 0};
    fwrite(&parekkkk, sizeof(uint8_t), 4, f);
    fclose(f);
    return 0;
}
