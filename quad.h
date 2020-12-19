#ifndef QUAD_H
#define QUAD_H

#include "scalpa.h"

#define GOTO_INCOMPLETE -1

enum instruction_t {
    GOTO_QUAD,
    AFF_QUAD,
    IF_QUAD,
    IF_LT_QUAD, 
    IF_GT_QUAD,
    IF_LT_EQ_QUAD, 
    IF_GT_EQ_QUAD, 
    IF_EQ_QUAD, 
    IF_DIFF_QUAD,
    OPB_PLUS_QUAD,
    OPB_MINUS_QUAD,
    OPB_STAR_QUAD,
    OPB_DIVIDE_QUAD,
    OPB_POW_QUAD,
    OPB_LT_QUAD,
    OPB_LT_EQ_QUAD,
    OPB_GT_QUAD,
    OPB_GT_EQ_QUAD,
    OPB_EQ_QUAD,
    OPB_DIFF_QUAD,
    OPU_MINUS_QUAD,
    OPU_NOT_QUAD,
    READ_QUAD,
    WRITE_QUAD,
    CALL_QUAD,
    CALL_AFF_QUAD,
    PARAM_QUAD,
    RETURN_UNIT_QUAD,
    RETURN_QUAD,
    EXIT_QUAD
};

struct quad_t {
    // {IF op1 _ target}
    // {IF_REL op1 op2 target}
    // {GOTO _ _ target}
    // {AFF res _ op1}
    // {OPB op1 op2 res}
    // {OPU res _ op1}
    // {READ/WRITE op1 _ _}
    // {PARAM op1 _ _}
    // {CALL_AFF op1 op2 res} // op1 := call op2, res (target = const nb of par)
    // {CALL op1 _ res} // call op1, res (res = const nb of par)
    // {return_unit _ _ _}
    // {return _ _ res}
    // {exit _ _ _}
    enum instruction_t instruction;
    struct expr_t op1;
    struct expr_t op2;
    union {
        struct expr_t res;
        int target;
    };
    int res_type; // type of union res=0, target=1
    int is_label;
};

struct quad_table_t {
    struct quad_t *quads;
    int nextquad;
    int table_size;
};

void init_quad_table();

void free_quad_table();

void display_quad_op(struct expr_t quad_op);

void display_instruction(int instr);

void display_quad(struct quad_t quad);

void display_quad_table();

struct quad_list_t {
    int position;
    struct quad_list_t* next;
};

struct quad_list_t* create_quad_list(int position);

struct quad_list_t* concat_quad_list(struct quad_list_t* list_1, 
                                     struct quad_list_t* list_2);

void complete_quad_list(struct quad_list_t* liste, int target);

void free_quad_list(struct quad_list_t* quad_list);

void free_quad_op(struct expr_t quad_op);

void free_quad(struct quad_t quad);

void free_quad_table();

int get_instr(int op, int is_unary);

void gencode (int instruction, 
              struct expr_t op1, 
              struct expr_t op2,
              struct expr_t res);

void gencode_goto (int target);

/**
 * @brief add an exit quad at the end of the quad table
 * @result void
 */
void add_exit();

/**
 * @brief set to 1 is_label attribut of each quad that is a target of a goto
 * @result void
 */
void complete_labels();

/**
 * @brief return a new temp ptr for quad generation
 * @param mode 0 normal behaviour (return a ptr), 1 reset the counter
 * @result new temp ptr
 */
int newtemp(int mode);

#endif