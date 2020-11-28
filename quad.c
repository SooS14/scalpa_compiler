#include <stdio.h>
#include <stdlib.h>
#include "quad.h"

extern struct quad_table_t quad_table;
extern struct symbol_table_t symbol_table;

void init_quad_table() {
    quad_table.table_size = INIT_TABLE_SIZE;
    quad_table.nextquad = 0;
    quad_table.quads = malloc(INIT_TABLE_SIZE * sizeof(struct quad_t));
    MCHECK(quad_table.quads);
}

void free_quad_table();

void display_instruction(int instr) {
    switch (instr) {
        case GOTO_QUAD:       printf("goto");    break;
        case AFF_QUAD:        printf(":=");      break;
        case IF_LT_QUAD:      printf("<");       break;
        case IF_GT_QUAD:      printf(">");       break;
        case IF_LT_EQ_QUAD:   printf("<=");      break;
        case IF_GT_EQ_QUAD:   printf(">=");      break;
        case IF_EQ_QUAD:      printf("==");      break;
        case IF_DIFF_QUAD:    printf("!=");      break;
        case OPB_PLUS_QUAD:   printf("+");       break;
        case OPB_MINUS_QUAD:  printf("-");       break;
        case OPB_STAR_QUAD:   printf("*");       break;
        case OPB_DIVIDE_QUAD: printf("/");       break;
        case OPB_POW_QUAD:    printf("^");       break;
        case OPB_LT_QUAD:     printf("<");       break;
        case OPB_LT_EQ_QUAD:  printf("<=");      break;
        case OPB_GT_QUAD:     printf(">");       break;
        case OPB_GT_EQ_QUAD:  printf(">=");      break;
        case OPB_EQ_QUAD:     printf("==");      break;
        case OPB_DIFF_QUAD:   printf("!=");      break;
        case OPB_AND_QUAD:    printf("and");     break;
        case OPB_OR_QUAD:     printf("or");      break;
        case OPB_XOR_QUAD:    printf("xor");     break;
        case OPU_MINUS_QUAD:  printf("-");       break;
        case OPU_NOT_QUAD:    printf("not");     break;
    }
}

void display_quad_op(struct quad_op_t quad_op) {
    switch (quad_op.quad_op_type) {
        case QO_CST  :
        printf(" %i ", quad_op.value.cst); break;
        case QO_VAR  :
        printf(" %s ", symbol_table.symbols[quad_op.value.ptr].ident); break;
        case QO_TEMP :
        printf(" %%T%i ", quad_op.value.temp_ptr); break;
        break;
    }
}

void display_quad(struct quad_t quad) {
    if (quad.instruction == IF_GT_QUAD || quad.instruction == IF_LT_QUAD ||
        quad.instruction == IF_LT_EQ_QUAD || quad.instruction == IF_GT_EQ_QUAD||
        quad.instruction == IF_EQ_QUAD || quad.instruction == IF_DIFF_QUAD) {
        printf("[if (");
        display_quad_op(quad.op1);
        display_instruction(quad.instruction);
        display_quad_op(quad.op2);
        printf(") then goto %i]\n", quad.target);
    }
    else if (quad.instruction == GOTO_QUAD) {
        printf("[goto %i]\n", quad.target);
    }
    else if (quad.instruction == AFF_QUAD) {
        printf("[");
        display_quad_op(quad.res);
        display_instruction(quad.instruction);
        display_quad_op(quad.op1);
        printf("]\n");
    }
    else if(quad.instruction == OPU_MINUS_QUAD ||
            quad.instruction == OPU_NOT_QUAD) {
        printf("[");
        display_quad_op(quad.res);
        printf(":= ");
        display_instruction(quad.instruction);
        display_quad_op(quad.op1);
        printf("]\n");
    }
    else{
        printf("[");
        display_quad_op(quad.res);
        printf(":=");
        display_quad_op(quad.op1);
        display_instruction(quad.instruction);
        display_quad_op(quad.op2);
        printf("]\n");
    }
}

void display_quad_table() {
    printf("\nTABLE OF QUADS : (number of  quad = %i)\n", quad_table.nextquad);
    for (int i = 0; i < quad_table.nextquad; i++) {
        display_quad(quad_table.quads[i]);
    }
}

struct quad_list_t* create_quad_list(int position) {
    struct quad_list_t* list = malloc(sizeof(struct quad_list_t));
    MCHECK(list);
    list->position = position;
    list->next = NULL;
    return list;
}

struct quad_list_t* concat_quad_list(struct quad_list_t* list_1, 
                                     struct quad_list_t* list_2) {
    struct quad_list_t* list_concat;
    if (list_1 != NULL) {
        list_concat = list_1;
    }
    else {
        if (list_2 != NULL) {
            list_concat = list_2;
        }
        else {
            list_concat = NULL;
        }
    }
    if (list_1 != NULL) {
        while (list_1->next!=NULL) {
            list_1 = list_1->next;
        }
        list_1->next = list_2;
    }
    return list_concat;
}

void complete_quad_list(struct quad_list_t* quad_list, int target) {
    quad_table.quads[quad_list->position].target = target;
    while (quad_list->next != NULL) {
        quad_list = quad_list->next;
        if (quad_table.quads[quad_list->position].instruction != AFF_QUAD) {
            quad_table.quads[quad_list->position].target = target;
        }
    }
}

void free_quad_list();

void gencode (int instruction, 
              struct quad_op_t op1, 
              struct quad_op_t op2,
              struct quad_op_t res) {
    //realloc todo
    quad_table.quads[quad_table.nextquad].quad_4 = (instruction == GOTO_QUAD ||
         instruction == IF_GT_QUAD || instruction == IF_LT_QUAD ||
         instruction == IF_LT_EQ_QUAD || instruction == IF_GT_EQ_QUAD ||
         instruction == IF_EQ_QUAD || instruction == IF_DIFF_QUAD);
    quad_table.quads[quad_table.nextquad].instruction = instruction;
    quad_table.quads[quad_table.nextquad].op1 = op1;
    quad_table.quads[quad_table.nextquad].op2 = op2;
    if (quad_table.quads[quad_table.nextquad].quad_4) {
        quad_table.quads[quad_table.nextquad].target = GOTO_INCOMPLETE;
    }
    else {
        quad_table.quads[quad_table.nextquad].res = res;
    }
    quad_table.nextquad ++;
}

int newtemp() {
    static int ptr = -1;
    ptr ++;
    return ptr;
}