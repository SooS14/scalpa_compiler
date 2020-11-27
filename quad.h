#ifndef QUAD_H
#define QUAD_H

#define GOTO_INCOMPLETE -1
#define NO_GOTO -2

enum instruction_t {
    GOTO_QUAD, 
    AFF_QUAD, 
    IF_LT_QUAD, 
    IF_GT_QUAD,
    IF_LT_EQ_QUAD, 
    IF_GT_EQ_QUAD, 
    IF_EQ_QUAD, 
    IF_DIFF_QUAD
};

enum quad_op_type_t {QO_CST, QO_NAME};

struct quad_op_t {
    enum quad_op_type_t quad_op_type;
    union {
        int cst;
        char *ident;
    } value;
};

struct quad_t {
    // IF op1 op2 target
    // GOTO NULL NULL target
    // AFF op1 op2
    enum instruction_t instruction;
    struct quad_op_t op1;
    struct quad_op_t op2;
    int target; // goto target (-1 if incomplete)
};

struct quad_table_t {
    struct quad_t *quads;
    int nextquad;
    int table_size;
};

void init_quad_table();

void free_quad_table();

void display_quad(struct quad_t);

void display_quad_table();

struct quad_list_t {
    int position;
    struct quad_list_t* next;
};

//TODO maybe use linked list ?
struct quad_list_t* create_quad_list(int position);

struct quad_list_t* concat_quad_list(struct quad_list_t* list_1, 
                                     struct quad_list_t* list_2);

void complete_quad_list(struct quad_list_t* liste, int target);

void free_quad_list();

void gencode(int instruction, struct quad_op_t op1, struct quad_op_t op2);


#endif