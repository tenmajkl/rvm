#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define ARITHMETIC(OP, ID) if (sscanf(instruction, OP" %i %i %i", &arg1, &arg2, &arg3) == 3) {\
        if (arg1 > 15) {\
            puts("register1 value too big");\
			return 0;\
        }\
        if (arg2 > 15) {\
            puts("register2 value too big");\
			return 0;\
        }\
        if (arg2 > 15) {\
            puts("register3 value too big");\
			return 0;\
        }\
        result[0] = ID; \
        result[1] = arg1; \
        result[2] = arg2; \
        result[3] = arg3; \
        return 1; \
    }\

typedef struct {
    char name[16];
    int action;
} Label;

int labelValue(Label labels[], int label_count, char* label)
{
    for (int index = 0; index < label_count; index++) {
        if (strcmp(labels[index].name, label)) {
            return labels[index].action;
        }
    }

    return -1;
}

int translateInstruction(char* instruction, Label labels[], int label_count, uint8_t result[])
{
    int arg1;   
    int arg2;
    int arg3;
    char sarg[16];

    if (strcmp(instruction, "hlt") == 0) {
        result[0] = 0;
        result[1] = 0;
        result[2] = 0;
        result[3] = 0;
        return 1;
    }

    if (sscanf(instruction, "srv %i %i", &arg1, &arg2) == 2) {
        if (arg1 > 15) {
            puts("register value too big");
			return 0;
        }
        if (arg2 > 255) {
            puts("value too big");
			return 0;
        }
        result[0] = 1;
        result[1] = arg1;
        result[2] = arg2 / 16; 
        result[3] = arg2 % 16;
        return 1;
    }

    ARITHMETIC("add", 2)
    ARITHMETIC("sub", 3)
    ARITHMETIC("mul", 4)
    ARITHMETIC("div", 5)
    ARITHMETIC("oor", 6)
    ARITHMETIC("and", 7)
    ARITHMETIC("xor", 8)

    if (sscanf(instruction, "jmp %i", &arg1) == 1) {
        if (arg1 > 63) {
            puts("position value too big");
			return 0;
        }

        result[0] = 9;
        result[1] = arg1 / 16; 
        result[2] = arg1 % 16;
        result[3] = 0;
        return 1;
    }

    if (sscanf(instruction, "jmp _%15s", sarg) == 1) {
        arg1 = labelValue(labels, label_count, sarg);

        if (arg1 == -1) {
            printf("Label %s doesnt exist", sarg);
            return 0;
        }

        result[0] = 9;
        result[1] = arg1 / 16; 
        result[2] = arg1 % 16;
        result[3] = 0;
        return 1;
    }

    if (sscanf(instruction, "jrz %i %i", &arg1, &arg2) == 2) {
        if (arg1 > 15) {
            puts("register value too big");
            return 0;
        }

        if (arg2 > 63) {
            puts("position value too big");
            return 0;
        }

        result[0] = 10;
        result[1] = arg1;
        result[2] = arg2 / 16; 
        result[3] = arg2 % 16;
        return 1;
    }

    if (sscanf(instruction, "jrz %i _%15s", &arg1, sarg) == 2) {
        if (arg1 > 15) {
            puts("register value too big");
            return 0;
        }

        arg2 = labelValue(labels, label_count, sarg);

        if (arg2 == -1) {
            printf("Label %s doesnt exist", sarg);
            return 0;
        }

        result[0] = 10;
        result[1] = arg1;
        result[2] = arg2 / 16; 
        result[3] = arg2 % 16;
        return 1;
    }

    if (sscanf(instruction, "set %i %i", &arg1, &arg2) == 2) {
        if (arg1 > 15) {
            puts("position value too big");
			return 0;
        }

        if (arg2 > 15) {
            puts("register value too big");
			return 0;
        }

        result[0] = 11;
        result[1] = arg1; 
        result[2] = arg2;
        result[3] = 0;
        return 1;
    }

    if (sscanf(instruction, "get %i %i", &arg1, &arg2) == 2) {
        if (arg1 > 15) {
            puts("position value too big");
			return 0;
        }

        if (arg2 > 15) {
            puts("register value too big");
			return 0;
        }

        result[0] = 12;
        result[1] = arg1; 
        result[2] = arg2;
        result[3] = 0;
        return 1;
    }

    if (sscanf(instruction, "out %i", &arg1) == 1) {
        if (arg1 > 15) {
            puts("register value too big");
			return 0;
        }

        result[0] = 13;
        result[1] = arg1; 
        result[2] = 0;
        result[3] = 0;
        return 1;
    }

    if (sscanf(instruction, "cht %i", &arg1) == 1) {
        if (arg1 > 15) {
            puts("register value too big");
			return 0;
        }

        result[0] = 14;
        result[1] = arg1; 
        result[2] = 0;
        result[3] = 0;
        return 1;
    }

    return 0;
}

void translate(FILE* from, FILE* to)
{
    char line[64];
    int action = 0;
    Label labels[16] = {};
    int label_count = 0;
    uint8_t instruction[4] = {};
    int i;
    while ((i = fscanf(from, "\n%[^\n]s\n", line)) == 1) { // i have no idea
        if (line[0] == 0) {
            continue;
        }

        if (line[0] == '#') {
            continue;
        }

        if (line[0] == '_') {
            labels[label_count].action = action;
            if (sscanf(line, "_%15s:", labels[label_count].name) != 1) {
                puts("label not syntacticaly correct");
		 	    return;
            }

            label_count++;
            continue;    
        }

        if (!translateInstruction(line, labels, label_count, instruction)) {
            printf("error at action %i", action);
            return;
        }
        //printf("%i %i %i %i\n", instruction[0], instruction[1], instruction[2], instruction[3]);
        fwrite(&instruction, sizeof(uint8_t), 4, to);
        action++;
    }

}

int main(int argc, char** argv)
{
    if (argc < 3) {
        return -1;
    }

    FILE* f = fopen(argv[1], "r");
    FILE* t = fopen(argv[2], "wb");
    if (f == NULL || t == NULL) {
        puts("file not found");
        return -1;
    }
    translate(f, t);
    fclose(f);
    fclose(t);
    return 0;
}
