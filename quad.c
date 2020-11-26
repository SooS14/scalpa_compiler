#include <stdio.h>
#include <stdlib.h>
#include "quad.h"
#include "scalpa.h"

extern struct quad_table_t quad_table;

void init_quad_table() {
    quad_table.table_size = INIT_TABLE_SIZE;
    quad_table.nextquad = 0;
    quad_table.quads = malloc(INIT_TABLE_SIZE * sizeof(struct quad_t));
    MCHECK(quad_table.quads);
}

void free_quad_table();

void display_quad(struct quad_t);

void display_quad_table();

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

void gencode(int instruction, struct quad_op_t op1, struct quad_op_t op2) {
    //realloc todo
    quad_table.quads[quad_table.nextquad].instruction = instruction;
    quad_table.quads[quad_table.nextquad].op1 = op1;
    quad_table.quads[quad_table.nextquad].op2 = op2;
    quad_table.quads[quad_table.nextquad].target = 
        (instruction == AFF_QUAD) ? NO_GOTO : GOTO_INCOMPLETE;
    quad_table.nextquad ++;
}