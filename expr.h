#ifndef EXPR_H
#define EXPR_H

#include "scalpa.h"
#include <string.h>
#include <math.h>

/**
 * write the string associated the operator code : op in str_op
 */
void get_operator_str (int op, char (*str_op)[5]);

/**
 * Compute and return the result of : expr1 opb expr2 
 * exit if expr1 or expr2 have a type that doesn't support opb
 * example : string + string, true < false, int xor int
 * exit if expr1 and expr2 have different type
 */
struct expr_t compute_opb_const_expr (struct expr_t expr1,
                                      struct expr_t expr2,
                                      int opb,
                                      int result_type);

/**
 * Compute and return the result of : opu expr
 * exit if expr have a type that doesn't support opu
 * example : opu string, - bool, not int
 */
struct expr_t compute_opu_const_expr (struct expr_t expr, int opu);

void copy_expr_t (struct expr_t *dest, struct expr_t *origin);

int get_index_array (int (*range_array)[2],
                     int len_range_list,
                     struct linked_list *exprlist);

#endif