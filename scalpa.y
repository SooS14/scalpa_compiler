%{
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include "scalpa.h"
#include "linked_list.h"
#include "table_of_symbol.h"
#include "args-parser.h"
#include "quad.h"
#include "expr.h"

#define DEBUG 1

struct symbol_table_t symbol_table;
struct quad_table_t quad_table;
struct string_table_t string_table;

int fd_out;

int scope_function = 0; // used to check if the return is general
int is_returned = 0;    // used to check if a function as a return

int yylex(void);

int yylex_destroy(void);

void yyerror(const char * msg);

extern int current_line;

/* -------------------------------------------------------------------------- */
/*                             write spim file                                */
/* -------------------------------------------------------------------------- */

void write_instr(int i) {
        struct quad_t quad;
        quad = quad_table.quads[i];

        if (quad_table.quads[i].is_label) {
            dprintf(fd_out, "__label_%i:\n", i);
        }
        if (DEBUG) {
            switch(quad.instruction) {
            case GOTO_QUAD :
                dprintf(fd_out, "#%i,\tGOTO_QUAD\n", i);        break;
            case AFF_QUAD :
                dprintf(fd_out, "#%i,\tAFF_QUAD\n", i);         break;
            case IF_QUAD :
                dprintf(fd_out, "#%i,\tIF_QUAD\n", i);          break;
            case IF_GT_QUAD :
                dprintf(fd_out, "#%i,\tIF_GT_QUAD\n", i);       break;
            case IF_LT_EQ_QUAD :
                dprintf(fd_out, "#%i,\tIF_LT_EQ_QUAD\n", i);    break;
            case IF_GT_EQ_QUAD :
                dprintf(fd_out, "#%i,\tIF_GT_EQ_QUAD\n", i);    break;
            case IF_EQ_QUAD :
                dprintf(fd_out, "#%i,\tIF_EQ_QUAD\n", i);       break;
            case IF_DIFF_QUAD :
                dprintf(fd_out, "#%i,\tIF_DIFF_QUAD\n", i);     break;
            case OPB_PLUS_QUAD:
                dprintf(fd_out, "#%i,\tOPB_PLUS_QUAD\n", i);    break;
            case OPB_MINUS_QUAD:
                dprintf(fd_out, "#%i,\tOPB_MINUS_QUAD\n", i);   break;
            case OPB_STAR_QUAD:
                dprintf(fd_out, "#%i,\tOPB_STAR_QUAD\n", i);    break;
            case OPB_DIVIDE_QUAD:
                dprintf(fd_out, "#%i,\tOPB_DIVIDE_QUAD\n", i);  break;
            case OPB_POW_QUAD:
                dprintf(fd_out, "#%i,\tOPB_POW_QUAD\n", i);     break;
            case OPU_MINUS_QUAD:
                dprintf(fd_out, "#%i,\tOPU_MINUS_QUAD\n", i);   break;
            case READ_QUAD:
                dprintf(fd_out, "#%i,\tREAD_QUAD\n", i);        break;
            case WRITE_QUAD:
                dprintf(fd_out, "#%i,\tWRITE_QUAD\n", i);       break;
            case CALL_QUAD:
                dprintf(fd_out, "#%i,\tCALL_QUAD\n", i);        break;
            case CALL_AFF_QUAD:
                dprintf(fd_out, "#%i,\tCALL_AFF_QUAD\n", i);    break;
            case PARAM_QUAD:
                dprintf(fd_out, "#%i,\tPARAM_QUAD\n", i);       break;
            case RETURN_UNIT_QUAD: 
                dprintf(fd_out, "#%i,\tRETURN_UNIT_QUAD\n", i); break;
            case RETURN_QUAD: 
                dprintf(fd_out, "#%i,\tRETURN_QUAD\n", i);      break;
            case EXIT_QUAD: 
                dprintf(fd_out, "#%i,\tEXIT_QUAD\n", i);        break;
            }
        }
        switch(quad.instruction) {
        case GOTO_QUAD :
            dprintf(fd_out, "\tj __label_%i\n",quad_table.quads[i].target);
            break;
        
        case AFF_QUAD :
            if (quad.op1.quad_op_type == QO_VAR && 
                quad.op1.is_array) {
                dprintf(fd_out, "\tla $t%i, %s_%i\n",
                    quad.res.temp.ptr,
                    symbol_table.symbols[quad.op1.var.ptr].ident,
                    symbol_table.symbols[quad.op1.var.ptr].scope);
                return;
            }
            switch(quad.res.quad_op_type) {
            case QO_VAR:
                switch(quad.op1.quad_op_type) {
                case QO_CST:
                    dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                    dprintf(fd_out, "\tsw $t9, %s_%i\n",
                        symbol_table.symbols[quad.res.var.ptr].ident,
                        symbol_table.symbols[quad.res.var.ptr].scope);
                    break;
                    
                case QO_TEMP:
                    dprintf(fd_out, "\tsw $t%i, %s_%i\n",
                        quad.op1.temp.ptr,
                        symbol_table.symbols[quad.res.var.ptr].ident,
                        symbol_table.symbols[quad.res.var.ptr].scope);
                    break;
                }
                break;
            
            case QO_TEMP:
                switch(quad.op1.quad_op_type) {
                case QO_CST:
                    if (quad.res.temp.symbol_type == ARRAY_TYPE && 
                        !quad.res.is_array) {
                        dprintf(fd_out, "\tadd $t%i, $t%i, $t%i\n",
                            quad.res.temp.ptr,
                            quad.res.temp.ptr,
                            quad.res.temp.ptr_to_index);
                        dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                        dprintf(fd_out, "\tsw $t9, ($t%i)\n",quad.res.temp.ptr);
                    }
                    else {
                        dprintf(fd_out, "\tli $t%i, %i\n",
                            quad.res.temp.ptr,
                            quad.op1.const_int);
                    }
                    break;
                
                case QO_VAR:
                    dprintf(fd_out, "\tlw $t%i, %s_%i\n",
                        quad.res.temp.ptr,
                        symbol_table.symbols[quad.op1.var.ptr].ident,
                        symbol_table.symbols[quad.op1.var.ptr].scope);
                    break;
                    
                case QO_TEMP:
                    if (quad.op1.temp.symbol_type == ARRAY_TYPE && 
                        !quad.op1.is_array) {
                        dprintf(fd_out, "\tadd $t%i, $t%i, $t%i\n",
                            quad.op1.temp.ptr,
                            quad.op1.temp.ptr,
                            quad.op1.temp.ptr_to_index);
                        dprintf(fd_out, "\tlw $t%i, ($t%i)\n",
                            quad.res.temp.ptr,
                            quad.op1.temp.ptr);
                    }
                    else if (quad.res.temp.symbol_type == ARRAY_TYPE && 
                            !quad.res.is_array) {
                        dprintf(fd_out, "\tadd $t%i, $t%i, $t%i\n",
                            quad.res.temp.ptr,
                            quad.res.temp.ptr,
                            quad.res.temp.ptr_to_index);
                        dprintf(fd_out, "\tsw $t%i, ($t%i)\n",
                            quad.op1.temp.ptr,
                            quad.res.temp.ptr);
                    }
                    else {
                        dprintf(fd_out, "\tlw $t%i, %s_%i\n",
                            quad.res.temp.ptr,
                            symbol_table.symbols[quad.op1.var.ptr].ident,
                            symbol_table.symbols[quad.op1.var.ptr].scope);
                    }
                    break;
                }
                break;
            }
            break;
        
        case IF_QUAD :
            if (quad.op1.quad_op_type == QO_CST) {
                dprintf(fd_out, "\tli $t9, 1\n");
                dprintf(fd_out, "\tli $t8, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tbeq $t8, $t9, __label_%i\n", 
                    quad_table.quads[i].target);
            }
            else {
                dprintf(fd_out, "\tli $t9, 1\n");
                dprintf(fd_out, "\tbeq $t%i, $t9, __label_%i\n",
                    quad.op1.temp.ptr,
                    quad_table.quads[i].target);
            }
            break;
        
        case IF_LT_QUAD :
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tblt $t%i, $t9, __label_%i\n",
                    quad.op1.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                     (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tblt $t9, $t%i, __label_%i\n",
                    quad.op2.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else {
                dprintf(fd_out, "\tblt $t%i, $t%i, __label_%i\n",
                    quad.op1.temp.ptr,
                    quad.op2.temp.ptr,
                    quad_table.quads[i].target);
            }
            break;
        
        case IF_GT_QUAD :
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tbgt $t%i, $t9, __label_%i\n",
                    quad.op1.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                     (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tbgt $t9, $t%i, __label_%i\n",
                    quad.op2.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else {
                dprintf(fd_out, "\tbgt $t%i, $t%i, __label_%i\n",
                    quad.op1.temp.ptr,
                    quad.op2.temp.ptr,
                    quad_table.quads[i].target);
            }
            break;
        
        case IF_LT_EQ_QUAD :
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tble $t%i, $t9, __label_%i\n",
                    quad.op1.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                     (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tble $t9, $t%i, __label_%i\n",
                    quad.op2.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else {
                dprintf(fd_out, "\tble $t%i, $t%i, __label_%i\n",
                    quad.op1.temp.ptr,
                    quad.op2.temp.ptr,
                    quad_table.quads[i].target);
            }
            break;
        
        case IF_GT_EQ_QUAD :
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tbge $t%i, $t9, __label_%i\n",
                    quad.op1.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                     (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tbge $t9, $t%i, __label_%i\n",
                    quad.op2.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else {
                dprintf(fd_out, "\tbge $t%i, $t%i, __label_%i\n",
                    quad.op1.temp.ptr,
                    quad.op2.temp.ptr,
                    quad_table.quads[i].target);
            }
            break;
        
        case IF_EQ_QUAD :
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tbeq $t%i, $t9, __label_%i\n",
                    quad.op1.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                     (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tbeq $t9, $t%i, __label_%i\n",
                    quad.op2.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else {
                dprintf(fd_out, "\tbeq $t%i, $t%i, __label_%i\n",
                    quad.op1.temp.ptr,
                    quad.op2.temp.ptr,
                    quad_table.quads[i].target);
            }
            break;
        
        case IF_DIFF_QUAD :
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tbne $t%i, $t9, __label_%i\n",
                    quad.op1.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                     (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tbne $t9, $t%i, __label_%i\n",
                    quad.op2.temp.ptr, 
                    quad_table.quads[i].target);
            }
            else {
                dprintf(fd_out, "\tbne $t%i, $t%i, __label_%i\n",
                    quad.op1.temp.ptr,
                    quad.op2.temp.ptr,
                    quad_table.quads[i].target);
            }
            break;
        
        case OPB_PLUS_QUAD:
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\taddi $t%i, $t%i, %i\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr,
                    quad.op2.const_int);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                    (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\taddi $t%i, $t%i, %i\n",
                    quad.res.temp.ptr,
                    quad.op2.temp.ptr,
                    quad.op1.const_int);
            }
            else {
                dprintf(fd_out, "\tadd $t%i, $t%i, $t%i\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr,
                    quad.op2.const_int);
            }
            break;

        case OPB_MINUS_QUAD:
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tsub $t%i, $t%i, $t9\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                    (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tsub $t%i, $t9, $t%i\n",
                    quad.res.temp.ptr,
                    quad.op2.temp.ptr);
            }
            else {
                dprintf(fd_out, "\tsub $t%i, $t%i, $t%i\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr,
                    quad.op2.const_int);
            }
            break;

        case OPB_STAR_QUAD:
            if ((quad.op1.quad_op_type == QO_CST) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t8, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tmulo $t%i, $t8, $t9\n", quad.res.temp.ptr);
            }
            else if ((quad.op1.quad_op_type == QO_TEMP) && 
                    (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tmulo $t%i, $t%i, $t9\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                    (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tmulo $t%i, $t9, $t%i\n",
                    quad.res.temp.ptr,
                    quad.op2.temp.ptr);
            }
            else {
                dprintf(fd_out, "\tmulo $t%i, $t%i, $t%i\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr,
                    quad.op2.temp.ptr);
            }
            break;

        case OPB_DIVIDE_QUAD:
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op2.const_int);
                dprintf(fd_out, "\tdiv $t%i, $t%i, $t9\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, %i\n", quad.op1.const_int);
                dprintf(fd_out, "\tdiv $t%i, $t9, $t%i\n",
                    quad.res.temp.ptr,
                    quad.op2.temp.ptr);
            }
            else {
                dprintf(fd_out, "\tdiv $t%i, $t%i, $t%i\n",
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr,
                    quad.op2.const_int);
            }
            break;

        case OPB_POW_QUAD:
            if ((quad.op1.quad_op_type == QO_TEMP) && 
                (quad.op2.quad_op_type == QO_CST)) {
                dprintf(fd_out, "\tli $t9, 0\n");
                dprintf(fd_out, "\tli $t%i, 1\n", quad.res.temp.ptr);
                dprintf(fd_out, "__pow_loop_%i: beq $t9, %i, __end_pow_%i\n",
                    i,quad.op2.const_int,i);
                dprintf(fd_out, "\tmulo $t%i, $t%i, $t%i\n",
                    quad.res.temp.ptr,
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr);
                dprintf(fd_out, "\taddi $t9, $t9, 1\n");
                dprintf(fd_out, "\tj __pow_loop_%i\n", i);
                dprintf(fd_out, "__end_pow_%i : li $t9, 0\n", i);
            }
            else if ((quad.op1.quad_op_type == QO_CST) &&
                (quad.op2.quad_op_type == QO_TEMP)) {
                dprintf(fd_out, "\tli $t9, 0\n");
                dprintf(fd_out, "\tli $t%i, 1\n", quad.res.temp.ptr);
                dprintf(fd_out, "__pow_loop_%i: beq $t9, $t%i, __end_pow_%i\n",
                    i, quad.op2.temp.ptr, i);
                dprintf(fd_out, "\tmulo $t%i, $t%i, %i\n",
                    quad.res.temp.ptr,
                    quad.res.temp.ptr,
                    quad.op1.const_int);
                dprintf(fd_out, "\taddi $t9, $t9, 1\n");
                dprintf(fd_out, "\tj __pow_loop_%i\n", i);
                dprintf(fd_out, "__end_pow_%i : li $t9, 0\n", i);
            }
            else {
                dprintf(fd_out, "\tli $t9, 0\n");
                dprintf(fd_out, "\tli $t%i, 1\n", quad.res.temp.ptr);
                dprintf(fd_out, "__pow_loop_%i: beq $t9, $t%i, __end_pow_%i\n",
                    i,quad.op2.temp.ptr,i);
                dprintf(fd_out, "\tmulo $t%i, $t%i, $t%i\n",
                    quad.res.temp.ptr,
                    quad.res.temp.ptr,
                    quad.op1.temp.ptr);
                dprintf(fd_out, "\taddi $t9, $t9, 1\n");
                dprintf(fd_out, "\tj __pow_loop_%i\n", i);
                dprintf(fd_out, "__end_pow_%i : li $t9, 0\n", i);
            }
            break;
        
        case OPU_MINUS_QUAD:
            dprintf(fd_out, "\tneg $t%i, $t%i\n",
                quad.res.temp.ptr,
                quad.op1.temp.ptr);
            break;
        
        case READ_QUAD:
            if (quad.op1.quad_op_type == QO_TEMP) {
                dprintf(fd_out, "\tli $v0, 5\n");
                dprintf(fd_out, "\tsyscall\n");
                dprintf(fd_out, "\tmove $v0, $t%i\n", quad.op1.temp.ptr);
            }
            else {
                dprintf(fd_out, "\tli $v0, 5\n");
                dprintf(fd_out, "\tsyscall\n");
                dprintf(fd_out, "\tsw $v0, %s_%i\n", 
                    symbol_table.symbols[quad.op1.var.ptr].ident,
                    symbol_table.symbols[quad.op1.var.ptr].scope);
            }
            break;
        
        case WRITE_QUAD:
            if (quad.op1.quad_op_type == QO_TEMP) {
                dprintf(fd_out, "\tli $v0, 1\n");
                dprintf(fd_out, "\tmove $a0, $t%i\n", quad.op1.temp.ptr);
                dprintf(fd_out, "\tsyscall\n");
            }
            else if (quad.op1.quad_op_type == QO_CST) {
                if (quad.op1.type == STRING) {
                    if (string_table.table_size <= 
                        string_table.last_ident_index + 1) {
                        string_table.table_size += INIT_TABLE_SIZE;
                        string_table.strings = realloc(string_table.strings,
                            string_table.table_size * sizeof(char *));
                        MCHECK(string_table.strings);
                    }
                    string_table.strings[string_table.last_ident_index] = 
                        quad.op1.const_string;
                    dprintf(fd_out, "\tli $v0, 4\n");
                    dprintf(fd_out, "\tla $a0, __printed_string_%i\n",
                        string_table.last_ident_index);
                    dprintf(fd_out, "\tsyscall\n");
                    string_table.last_ident_index ++;
                }
                else if (quad.op1.type == INT) {
                    dprintf(fd_out, "\tli $v0, 1\n");
                    dprintf(fd_out, "\tli $a0, %i\n", quad.op1.const_int);
                    dprintf(fd_out, "\tsyscall\n");
                }
            }
            break;

        case CALL_QUAD:
            dprintf(fd_out, "\tjal __func_label_%s\n", 
                symbol_table.symbols[quad.op1.var.ptr].ident);
            break;
        case CALL_AFF_QUAD:
            dprintf(fd_out, "\tjal __func_label_%s\n", 
                symbol_table.symbols[quad.op2.var.ptr].ident);
            dprintf(fd_out, "\tmove $t%i, $v0\n", quad.op1.temp.ptr);
            break;

        case PARAM_QUAD:
            dprintf(fd_out, "\taddiu $sp, $sp, -4\n");
            dprintf(fd_out, "\tsw $t%i, 0($sp)\n", quad.op1.temp.ptr);
            break;

        case RETURN_UNIT_QUAD:
            dprintf(fd_out, "\tjr $ra\n\n");
            break;

        case RETURN_QUAD:
            dprintf(fd_out, "\tmove $v0, $t%i\n", quad.res.temp.ptr);
            dprintf(fd_out, "\tjr $ra\n\n");
            break;

        case EXIT_QUAD:
            dprintf(fd_out, "\tli $v0, 10\n");
            dprintf(fd_out, "\tsyscall\n\n");
            break;
    }
}

void write_main(char * program_name) {
    dprintf(fd_out, "# program : %s\n\t.text\n\t.globl main\nmain:\n",
        program_name);

    string_table.table_size = INIT_TABLE_SIZE;
    string_table.last_ident_index = 0;
    string_table.strings = malloc(INIT_TABLE_SIZE * sizeof(char *));
    MCHECK(symbol_table.symbols);

    for (int i = symbol_table.quad_main ; i < quad_table.nextquad ; i++) {
        dprintf(fd_out, "\n");
        write_instr(i);
    }
}

void write_functions() {
    struct symbol_t symbol;
    int quad_start = 0;
    int quad_end = 0;
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        symbol = symbol_table.symbols[i];
        if (symbol.var_func_par == FUNC_T) {
            quad_start = symbol.type.func.quad_start;
            quad_end = symbol.type.func.quad_end;
            dprintf(fd_out, "__func_label_%s:\n",symbol.ident);
            for (int j = symbol.type.func.nb_param; j > 0 ; j--)  {
                struct param_t parame = symbol_table.symbols[i+j].type.param;
                if (parame.typename->symbol_type == ARRAY_TYPE) {
                    int nb_element = 1;
                    for (int j = 0; j < parame.typename->len_range_list; j++) {
                        nb_element *= (parame.typename->range_array[j][1] 
                            - parame.typename->range_array[j][0] + 1); 
                    }
                    dprintf(fd_out, "\tlw $t9, 0($sp)\n");
                    dprintf(fd_out, "\tla $t8, %s_%i\n",
                        symbol_table.symbols[i+j].ident,
                        symbol_table.symbols[i+j].scope);

                    // loop to copy array
                    dprintf(fd_out, "\tli $t0, %i\n", nb_element);
                    dprintf(fd_out, "\tli $t1, 0\n");
                    dprintf(fd_out, "\t__label_%s_%i:\n", symbol.ident, j);
                    dprintf(fd_out, "\tbgt $t1, $t0, __label_%s_%i_end\n",
                        symbol.ident, j);
                    dprintf(fd_out, "\tlw $t7, 0($t9)\n");
                    dprintf(fd_out, "\tsw $t7, ($t8) \n");
                    dprintf(fd_out, "\taddiu $t9, $t9, 4\n");
                    dprintf(fd_out, "\taddiu $t8, $t8, 4\n");
                    dprintf(fd_out, "\taddi $t1, $t1, 1\n");
                    dprintf(fd_out, "\tj __label_%s_%i\n", symbol.ident, j);
                    dprintf(fd_out, "\t__label_%s_%i_end:\n", symbol.ident, j);

                    dprintf(fd_out, "\taddiu $sp, $sp, 4\n");
                }
                else {
                    dprintf(fd_out, "\tlw $t9, 0($sp)\n");
                    dprintf(fd_out, "\tsw $t9, %s_%i\n",
                        symbol_table.symbols[i+j].ident,
                        symbol_table.symbols[i+j].scope);
                    dprintf(fd_out, "\taddiu $sp, $sp, 4\n");
                }
            }
            for (int j = quad_start; j <= quad_end; j++) {
                write_instr(j);
                dprintf(fd_out, "\n");
            }
        }
    }
}

void write_data() {
    dprintf(fd_out, "\n\t.data\n");
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        struct typename_t *cur_typename;
        if (symbol_table.symbols[i].var_func_par == VAR_T) {
            cur_typename = symbol_table.symbols[i].type.var.typename;
        }
        else if (symbol_table.symbols[i].var_func_par == PARAM_T) {
            cur_typename = symbol_table.symbols[i].type.param.typename;
        }
        else {
            continue;
        }
        if (cur_typename->symbol_type == ATOMIC_TYPE) {
            dprintf(fd_out, "\t%s_%i:\t.word 0\n",
                symbol_table.symbols[i].ident,
                symbol_table.symbols[i].scope);
        }
        else {
            int nb_element = 1;
            for (int j = 0; j < cur_typename->len_range_list; j++) {
                nb_element *= (cur_typename->range_array[j][1] 
                    - cur_typename->range_array[j][0] + 1); 
            }
            dprintf(fd_out, "\t%s_%i:\t.word ",
                symbol_table.symbols[i].ident,
                symbol_table.symbols[i].scope);
            for (int j = 0; j < nb_element -1; j++) {
                dprintf(fd_out, "0, ");
            }
            dprintf(fd_out, "0\n");
        }
    }
    for (int i = 0; i < string_table.last_ident_index; i++) {
        dprintf(fd_out, "\t__printed_string_%i: .asciiz %s\n",i , 
            string_table.strings[i]);
    }
}

%}
%code requires {
    #include "linked_list.h"
    #include "scalpa.h"
    #include "table_of_symbol.h"
    #include "quad.h"
}

%union {
    struct expr_t expr_u;
    int int_u;
    char *str_u;
    struct linked_list *list_u;
    struct typename_t *typename_u;
    struct param_t par_u;
    struct vardecl_t *vardecl_u;
    struct lvalue_t lvalue_u;
    struct quad_list_t *quad_list_u;
}

%token <expr_u> CTE
%token <str_u> IDENT
%type <expr_u> expr
%type <int_u> opb opu mark atomictype integer
%type <typename_u> typename arraytype
%type <list_u> identlist rangelist parlist vardecllist exprlist
%type <quad_list_u> instr sequence
%type <vardecl_u> varsdecl
%type <lvalue_u> lvalue
%type <par_u> par

%token '(' ')' '[' ']' ',' ';' ':' ASSIGNMENT FUNCTION REF READ WRITE BEGIN_
%token END IF RETURN WHILE DO
%token VAR UNIT_TYPE BOOL_TYPE INT_TYPE
%token OF ARRAY RANGELIST_SEPARATOR PROGRAM

%left OPB
%left OPU
%left OP_MINUS OPU_NOT
%left OPB_POW
%left OPB_STAR OPB_DIVIDE OPB_AND
%left OPB_PLUS OPB_OR OPB_XOR
%left OPB_L_EQ OPB_L OPB_G_EQ OPB_G OPB_EQ OPB_DIFF

%right THEN ELSE
%start program

%%

program :
    PROGRAM IDENT vardecllist {
        add_vardecllist_table($3);
        }
    fundecllist {
        symbol_table.cur_symbol_scope = 0;
        symbol_table.quad_main = quad_table.nextquad;
        } 
    instr {
        complete_quad_list($7, quad_table.nextquad);
        free_quad_list($7);
        newtemp(1);
        add_exit();
        complete_labels();
        write_main($2);
        write_functions();
        write_data();
        free($2);
        }

/* -------------------------------------------------------------------------- */
/*                         variable declaration                               */
/* -------------------------------------------------------------------------- */

vardecllist : 
      /* empty */ {$$ = list_init();}
    | varsdecl {
        $$ = list_init(); 
        list_push($$, $1, sizeof(struct vardecl_t));
        free($1);
        }
    | varsdecl ';' vardecllist {
        $$ = $3;
        list_push($$, $1, sizeof(struct vardecl_t));
        free($1);
        }

varsdecl :
    VAR identlist ':' typename {$$ = create_vardecl($2, $4);}

identlist :
      IDENT {
        $$ = list_init();
        list_push($$, $1, strlen($1)+1);
        free($1);
        }
    | IDENT ',' identlist {
        list_push($3, $1, strlen($1)+1);
        $$ = $3;
        free($1);
        }

typename :
      atomictype {$$ = create_typename_atomic($1);}
    | arraytype  {$$ = $1;}

atomictype :
      UNIT_TYPE  {$$ = VOID_A;}
    | BOOL_TYPE  {$$ = BOOL_A;}
    | INT_TYPE   {$$ = INT_A;}

arraytype :
    ARRAY '[' rangelist ']' OF atomictype {$$ = create_typename_array($3, $6);}

rangelist :
      integer RANGELIST_SEPARATOR integer {
        if ($1 > $3) {
            handle_error("[%i..%i], invalid rangelist (%i > %i)",
                $1, $3, $1, $3);
        }
        $$ = list_init();
        int x1 = $1;
        int x2 = $3;
        list_push($$, &x2, sizeof(int));
        list_push($$, &x1, sizeof(int));
        }
    | integer RANGELIST_SEPARATOR integer ',' rangelist {
        if ($1 > $3) {
            handle_error("[%i..%i], invalid rangelist (%i > %i)",
                $1, $3, $1, $3);
        }
        $$ = $5;
        int x1 = $1;
        int x2 = $3;
        list_push($$, &x2, sizeof(int));
        list_push($$, &x1, sizeof(int));
        }

integer :
    expr {
        if ($1.type == INT && $1.quad_op_type == QO_CST) {
            $$ = $1.const_int;
        }
        else {
            handle_error("elements of a rangelist for array declaration "
                "must be integers\n");
        }
        }

/* -------------------------------------------------------------------------- */
/*                         function declaration                               */
/* -------------------------------------------------------------------------- */

fundecllist : 
      /* empty */
    | fundecl ';' fundecllist

fundecl : FUNCTION IDENT '(' parlist ')' ':' atomictype vardecllist {
        add_func_ident_table($2, $7, $4);
        add_paramlist_table($4);
        add_vardecllist_table($8);
        is_returned = 0;
        } 
    instr mark {
        if (!is_returned && $7 != VOID_A) {
            handle_error("no return in function [%s]", $2);
        }
        int ptr = is_symbol_in_table($2, 0);
        symbol_table.symbols[ptr].type.func.quad_end = $11;
        if (!is_returned) {
            symbol_table.symbols[ptr].type.func.quad_end ++;
            struct expr_t res;
            gencode (RETURN_UNIT_QUAD, res, res, res);
            complete_quad_list($10, $11+1);
        }
        else {
            complete_quad_list($10, $11);
        }
        free_quad_list($10);
        newtemp(1);
        }

parlist : 
      /* empty */ {$$ = NULL;}
    | par {
        $$ = list_init();
        list_push($$, &$1, sizeof(struct param_t));
        }
    | par ',' parlist {
        list_push($3, &$1, sizeof(struct param_t));
        $$ = $3;
        }

par :
      IDENT ':' typename {
        if ($3->atomic_type == VOID_A) {
            handle_error("parameter can't be of type unit");
        }
        $$.ident = malloc(strlen($1)+1);
        MCHECK($$.ident);
        strncpy($$.ident, $1, strlen($1)+1);
        $$.ref = 0;
        $$.typename = $3;
        free($1);
        }
    | REF IDENT ':' typename {
        if ($4->atomic_type == VOID_A) {
            handle_error("parameter can't be of type ref unit");
        }
        $$.ident = malloc(strlen($2)+1);
        MCHECK($$.ident);
        strncpy($$.ident, $2, strlen($2)+1);
        $$.ref = 1;
        $$.typename = $4;
        free($2);
        }

/* -------------------------------------------------------------------------- */
/*                            instruction                                     */
/* -------------------------------------------------------------------------- */

instr :
      IF expr THEN {newtemp(1);} mark instr {
        if ($2.type != BOOL) {
            handle_error("if/then condition need be a boolean type");
        }
        complete_quad_list($2.true, $5);
        free_quad_list($2.true);
        $$ = concat_quad_list($2.false, $6);
        }
    | IF expr THEN {newtemp(1);} mark instr mark ELSE {newtemp(1);} mark instr{
        if ($2.type != BOOL) {
            handle_error("if/then/else condition need be a boolean type");
        }
        complete_quad_list($2.true, $5);
        free_quad_list($2.true);
        complete_quad_list($2.false, $10);
        free_quad_list($2.false);
        struct quad_list_t *temp;
        temp = concat_quad_list($6, create_quad_list($7));
        $$ = concat_quad_list(temp, $11);
        }
    | WHILE mark expr DO {newtemp(1);} mark instr {
        if ($3.type != BOOL) {
            handle_error("while condition need be a boolean type");
        }
        complete_quad_list($3.true, $6);
        free_quad_list($3.true);
        complete_quad_list($7, $2);
        free_quad_list($7);
        gencode_goto ($2);
        $$ = $3.false;
        }
    | lvalue ASSIGNMENT expr {
        if ($3.type == STRING) {
            handle_error("strings can't be assigned to variables", 
                $3.const_string);
        }
        if ($1.symbol_type != ARRAY_TYPE) {
            if (symbol_table.symbols[$1.ptr].var_func_par == PARAM_T) {
                if ($3.type != symbol_table.symbols[$1.ptr].type.param
                        .typename->atomic_type) {
                    handle_error("type of expr doesn't match in assigment "
                        "to [%s]", symbol_table.symbols[$1.ptr].ident);
                }
            }
            else {
                if ($3.type != symbol_table.symbols[$1.ptr].type.var
                        .typename->atomic_type) {
                    handle_error("type of expr doesn't match in assigment "
                        "to [%s]", symbol_table.symbols[$1.ptr].ident);
                }
            }
        }
        else if ($1.type != $3.type) {
            handle_error("type of expr doesn't match in assigment to array");
        }
        if ($3.is_array) {
            handle_error("Can't asign an array to a lvalue");
        }
        struct expr_t res;
        res.is_array = 0;
        if ($1.symbol_type == ARRAY_TYPE) {
            res.temp = $1;
            res.quad_op_type = QO_TEMP;
        }
        else {
            res.var = $1;
            res.quad_op_type = QO_VAR;
        }
        res.type = $3.type;
        if (res.type == BOOL) {
            struct expr_t temp;
            temp.is_array = 0;
            temp.quad_op_type = QO_CST;
            temp.type = BOOL;
            temp.const_bool = 1;
            complete_quad_list($3.true, quad_table.nextquad);
            free_quad_list($3.true);
            gencode (AFF_QUAD, temp, temp, res);
            temp.const_bool = 0;
            gencode_goto (quad_table.nextquad+2);
            complete_quad_list($3.false, quad_table.nextquad);
            free_quad_list($3.false);
            gencode (AFF_QUAD, temp, temp, res);
        }
        else {
            gencode (AFF_QUAD, $3, $3, res);
        }
        $$ = NULL;
        }
    | RETURN expr {
        int func_ptr = symbol_table.cur_symbol_scope;
        if (symbol_table.symbols[func_ptr].type.func.atomic_type != 
            $2.type) {
            handle_error("function [%s] and return expr have different types", 
                symbol_table.symbols[func_ptr].ident);
        }
        if ($2.is_array) {
            handle_error("Can't return an array");
        }
        if (scope_function == 1) {
            is_returned = 1;
        }
        if ($2.quad_op_type == QO_TEMP) {
            $2.temp.symbol_type = ATOMIC_TYPE;
        }
        else if ($2.quad_op_type == QO_VAR) {
            $2.var.symbol_type = ATOMIC_TYPE;
        }
        if ($2.type == BOOL && $2.quad_op_type != QO_CST) {
            struct expr_t temp;
            temp.is_array = 0;
            temp.quad_op_type = QO_CST;
            temp.type = BOOL;
            temp.const_bool = 1;
            complete_quad_list($2.true, quad_table.nextquad);
            free_quad_list($2.true);
            gencode (AFF_QUAD, temp, temp, $2);
            temp.const_bool = 0;
            gencode_goto (quad_table.nextquad+2);
            complete_quad_list($2.false, quad_table.nextquad);
            free_quad_list($2.false);
            gencode (AFF_QUAD, temp, temp, $2);
        }
        if ($2.type == BOOL && $2.quad_op_type == QO_CST) {
            complete_quad_list($2.true, quad_table.nextquad);
            free_quad_list($2.true);
            complete_quad_list($2.false, quad_table.nextquad);
            free_quad_list($2.false);
        }
        gencode (RETURN_QUAD, $2, $2, $2);
        $$ = NULL;
        }
    | RETURN {
        struct expr_t res;
        int func_ptr = symbol_table.cur_symbol_scope;
        if (symbol_table.symbols[func_ptr].type.func.atomic_type != VOID_A) {
            handle_error("function [%s] isn't of unit type", 
                symbol_table.symbols[func_ptr].ident);
        }
        if (scope_function == 1) {
            is_returned = 1;
        }
        gencode (RETURN_UNIT_QUAD, res, res, res);
        $$ = NULL;
        }
    | IDENT '(' exprlist ')' {
        int ptr = is_symbol_in_table($1, 0);
        if (ptr == -1) {
            handle_error("function [%s] is not declared", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[ptr];
        if (cur_symbol.var_func_par != FUNC_T) {
            handle_error("invalide use of [%s], [%s] isn't a function", $1, $1);
        }
        int nb_parameters = list_len($3);
        if (cur_symbol.type.func.nb_param != nb_parameters) {
            handle_error("arguments given in call of function [%s] are "
                "different from [%s] type", $1, $1);
        }
        for (int i = 0; i < nb_parameters; i++) {
            struct expr_t *expr_temp = (struct expr_t *)list_get_first($3);
            struct param_t cur_param = 
                symbol_table.symbols[ptr + 1 + i].type.param;
            if (cur_param.typename->atomic_type != expr_temp->type) {
                handle_error("arguments given in call of function [%s] are "
                    "different from [%s] type, for parameter [%i]", $1, $1,i+1);
            }
            if (cur_param.typename->symbol_type == ARRAY_TYPE) {
                if (cur_param.typename->symbol_type == ARRAY_TYPE && 
                    !expr_temp->is_array) {
                    handle_error("arguments given in call of function [%s] are "
                        "different from [%s] type, for parameter [%i]", 
                        $1, $1,i+1);
                }
                struct typename_t *arraytype_2;
                switch (symbol_table.symbols[expr_temp->index_symbol_table]
                        .var_func_par) {
                case VAR_T:
                    arraytype_2 = 
                        symbol_table.symbols[expr_temp->index_symbol_table]
                        .type.var.typename;
                    break;
                case PARAM_T:
                    arraytype_2 = 
                        symbol_table.symbols[expr_temp->index_symbol_table]
                        .type.param.typename;
                    break;
                default:
                    handle_error("arguments given in call of function [%s] are "
                    "different from [%s] type, for parameter [%i]", $1, $1,i+1);
                    break;
                }
                if (cur_param.typename->len_range_list != 
                    arraytype_2->len_range_list) {
                    handle_error("arguments given in call of function [%s] are "
                    "different from [%s] type, for parameter [%i]", $1, $1,i+1);
                }
                for (int i = 0; i < arraytype_2->len_range_list; i++) {
                    if ((arraytype_2->range_array[i][0] != 
                        cur_param.typename->range_array[i][0]) ||
                        (arraytype_2->range_array[i][1] != 
                        cur_param.typename->range_array[i][1])) {
                        handle_error("arguments given in call of function [%s]"
                            "are different from [%s] type, for parameter [%i]", 
                            $1, $1,i+1);
                    }
                }
            }
            gencode(PARAM_QUAD, *expr_temp, *expr_temp, *expr_temp);
            list_pop($3);
        }
        list_free($3);
        struct expr_t expr_func;
        expr_func.quad_op_type = QO_VAR;
        expr_func.type = INT;
        expr_func.var.ptr = ptr;
        expr_func.true = NULL;
        expr_func.false = NULL;
        expr_func.var.symbol_type = ATOMIC_TYPE;
        expr_func.var.ptr_to_index = 0;

        struct expr_t result;
        result.type = INT;
        result.true = NULL;
        result.false = NULL;
        result.quad_op_type = QO_CST;
        result.const_int = nb_parameters;
        gencode(CALL_QUAD, expr_func, expr_func, result);
        $$ = NULL;
        free($1);
        }
    | IDENT '(' ')' {
        int ptr = is_symbol_in_table($1, 0);
        if (ptr == -1) {
            handle_error("function [%s] is not declared", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[ptr];
        if (cur_symbol.var_func_par != FUNC_T) {
            handle_error("invalide use of [%s], [%s] isn't a function", $1, $1);
        }
        int nb_parameters = 0;
        if (cur_symbol.type.func.nb_param != nb_parameters) {
            handle_error("arguments given in call of function [%s] are "
                "different from [%s] type", $1, $1);
        }
        struct expr_t expr_func;
        expr_func.quad_op_type = QO_VAR;
        expr_func.true = NULL;
        expr_func.false = NULL;
        expr_func.type = INT;
        expr_func.var.ptr = ptr;
        expr_func.var.symbol_type = ATOMIC_TYPE;
        expr_func.var.ptr_to_index = 0;

        struct expr_t result;
        result.type = INT;
        result.true = NULL;
        result.false = NULL;
        result.quad_op_type = QO_CST;
        result.const_int = nb_parameters;
        gencode(CALL_QUAD, expr_func, expr_func, result);
        $$ = NULL;
        free($1);
        }
    | BEGIN_ {scope_function++;} sequence END {
        scope_function--; $<quad_list_u>$ = $3;
        }
    | BEGIN_ END {$$ = NULL;}
    | READ lvalue {
        struct expr_t op1;
        if ($2.symbol_type == ARRAY_TYPE) {
            op1.temp = $2;
            op1.quad_op_type = QO_TEMP;
            op1.is_array = 0;
            op1.type = INT;
            struct expr_t array_temp;
            array_temp.is_array = 0;
            array_temp.index_symbol_table = 0;
            array_temp.quad_op_type = QO_TEMP;
            array_temp.temp.ptr = newtemp(0);
            array_temp.temp.symbol_type = ATOMIC_TYPE;
            array_temp.temp.ptr_to_index = 0;
            gencode (READ_QUAD, array_temp, array_temp, array_temp);
            gencode (AFF_QUAD, array_temp, array_temp, op1);
        }
        else {
            op1.is_array = 0;
            op1.var = $2;
            op1.type = 
                symbol_table.symbols[$2.ptr].type.var.typename->atomic_type;
            op1.quad_op_type = QO_VAR;
            if (symbol_table.symbols[$2.ptr].var_func_par == PARAM_T) {
                op1.type = 
                    symbol_table.symbols[$2.ptr].type.param
                    .typename->atomic_type;
            }
            else if (symbol_table.symbols[$2.ptr].var_func_par == VAR_T){
                op1.type = 
                    symbol_table.symbols[$2.ptr].type.var.typename->atomic_type;
            }
            else {
                handle_error("Can't read a function");
            }
                gencode (READ_QUAD, op1, op1, op1);
        }
        $$ = NULL;
        }
    | WRITE expr {
        if ($2.is_array) {
            handle_error("Can't write an array");
        }
        if ($2.type == BOOL) {
            complete_quad_list($2.true, quad_table.nextquad);
            free_quad_list($2.true);
            complete_quad_list($2.false, quad_table.nextquad+2);
            free_quad_list($2.false);

            struct expr_t temp;
            char *true_str = malloc(7);
            true_str[0] = '\0';
            check_snprintf(snprintf(true_str, 7, "\"true\""), 7);
            MCHECK(true_str);
            char *false_str = malloc(8);
            check_snprintf(snprintf(false_str, 8, "\"false\""), 8);
            MCHECK(false_str);

            temp.is_array = 0;
            temp.quad_op_type = QO_CST;
            temp.type = STRING;
            temp.const_string = true_str;
            gencode (WRITE_QUAD, temp, temp, temp);
            gencode_goto (quad_table.nextquad+2);
            temp.const_string = false_str;
            gencode (WRITE_QUAD, temp, temp, temp);
        }
        else {
            gencode (WRITE_QUAD, $2, $2, $2);
        }
        $$ = NULL;
        }

sequence :
      instr ';' {newtemp(1);} mark sequence {
        complete_quad_list($1, $4);
        free_quad_list($1);
        $$ = $5;
        }
    | instr ';' {$$ = $1; newtemp(1);}
    | instr     {$$ = $1; newtemp(1);}

/* -------------------------------------------------------------------------- */
/*                            expression                                      */
/* -------------------------------------------------------------------------- */

lvalue :
      IDENT {
        int scope = symbol_table.cur_symbol_scope;
        if (($$.ptr = is_symbol_in_table($1, scope)) == -1) {
            handle_error("variable [%s] is not declared in this scope", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[$$.ptr];
        if (cur_symbol.var_func_par == FUNC_T) {
            handle_error("invalide use of [%s], [%s] is a function", $1, $1);
        }
        if (cur_symbol.var_func_par == PARAM_T && 
            cur_symbol.type.param.typename->symbol_type == ARRAY_TYPE) {
            handle_error("invalide use of variable, [%s] is an array", $1);
        }
        else if (cur_symbol.var_func_par == VAR_T && 
            cur_symbol.type.var.typename->symbol_type == ARRAY_TYPE) {
            handle_error("invalide use of variable, [%s] is an array", $1);
        }
        $$.symbol_type = ATOMIC_TYPE;
        $$.ptr_to_index = 0;
        free($1);
        }
    | IDENT '[' exprlist ']' {
        int scope = symbol_table.cur_symbol_scope;
        int ptr;
        if ((ptr = is_symbol_in_table($1, scope)) == -1) {
            handle_error("variable [%s] is not declared in this scope", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[ptr];
        if (cur_symbol.var_func_par == FUNC_T) {
            handle_error("invalide use of [%s], [%s] is a function", $1, $1);
        }
        struct expr_t array_ident;
        array_ident.is_array = 1;
        array_ident.index_symbol_table = ptr;
        array_ident.quad_op_type = QO_VAR;
        if (cur_symbol.var_func_par == PARAM_T) {
            array_ident.type = cur_symbol.type.param.typename->atomic_type;
            $$.type = cur_symbol.type.param.typename->atomic_type;
        }
        else {
            array_ident.type = cur_symbol.type.var.typename->atomic_type;
            $$.type = cur_symbol.type.var.typename->atomic_type;
        }
        array_ident.var.ptr = ptr;
        array_ident.var.symbol_type = ARRAY_TYPE;
        array_ident.var.ptr_to_index = 0;

        struct expr_t array_temp;
        $$.ptr = newtemp(0);
        $$.symbol_type = ARRAY_TYPE;
        if (cur_symbol.var_func_par == PARAM_T) {
            $$.ptr_to_index = 
                get_index_array (
                    cur_symbol.type.param.typename->range_array,
                    cur_symbol.type.param.typename->len_range_list, $3);
        }
        else {
            $$.ptr_to_index =
                get_index_array (
                    cur_symbol.type.var.typename->range_array,
                    cur_symbol.type.var.typename->len_range_list, $3);
        }
        array_temp.temp = $$;
        array_temp.is_array = 1;
        array_temp.index_symbol_table = ptr;
        array_temp.quad_op_type = QO_TEMP;
        array_temp.type = array_ident.type;

        gencode (AFF_QUAD, array_ident, array_ident, array_temp);

        list_free($3);
        free($1);
        }

exprlist :
      expr {
        $$ = list_init();
        list_push($$, &$1, sizeof(struct expr_t));
        }
    | expr ',' exprlist {
        list_push($3, &$1, sizeof(struct expr_t));
        $$ = $3;
        }

expr :
      CTE {
        $$ = $1; 
        $$.is_array = 0;
        if ($$.type == BOOL) {
            $$.true = create_quad_list(quad_table.nextquad);
            gencode(IF_QUAD, $$, $$, $$);
            $$.false = create_quad_list(quad_table.nextquad);
            gencode(GOTO_QUAD, $$, $$, $$);
        }
        }
    | '(' expr ')' {$$ = $2;}
    | expr opb mark expr %prec OPB {
        if ($1.type == STRING || $4.type == STRING) {
            handle_error("No operation allowed for type string");
        }
        if ($1.type != $4.type) {
            handle_error("No operation allowed between int and bool"
            " : expr1 is type [%s] and expr2 is type [%s]", 
            $1.type == INT ? "int" : "bool", $4.type == INT ? "int" : "bool");
        }
        if ($1.is_array || $4.is_array) {
            handle_error("No operation allowed for arrays");
        }
        char str_op[5];
        if ($1.type == INT) {
            if ($2 == OPB_PLUS || $2 == OP_MINUS ||$2 == OPB_STAR || 
                $2 == OPB_DIVIDE || $2 == OPB_POW) {
                $$.type = INT;
            }
            else if ($2 == OPB_L || $2 == OPB_L_EQ || $2 == OPB_G || 
                $2 == OPB_G_EQ || $2 == OPB_EQ || $2 == OPB_DIFF) {
                $$.type = BOOL;
            }
            else {
                get_operator_str($2, &str_op);
                handle_error("binary operator [%s] forbidden between int",
                    str_op);
            }
        }
        else {
            if ($2 == OPB_EQ || $2 == OPB_DIFF || $2 == OPB_AND || 
                $2 == OPB_AND ||$2 == OPB_OR || $2 == OPB_XOR) {
                $$.type = BOOL;
            }
            else {
                get_operator_str($2, &str_op);
                handle_error("binary operator [%s] forbidden between bool",
                    str_op);
                break;
            }
        }
        if ($1.quad_op_type == QO_CST && $4.quad_op_type == QO_CST &&
            $1.type != BOOL) {
            $$ = compute_opb_const_expr ($1, $4, $2, $$.type);
            if ($$.type == BOOL) {
                $$.true = create_quad_list(quad_table.nextquad);
                gencode(IF_QUAD, $$, $$, $$);
                $$.false = create_quad_list(quad_table.nextquad);
                gencode(GOTO_QUAD, $$, $$, $$);
            }
        }
        else {
            if ($1.type == INT) {
                if ($2 == OPB_PLUS || $2 == OP_MINUS || $2 == OPB_STAR ||
                        $2 == OPB_DIVIDE || $2 == OPB_POW) {
                    $$.is_array = 0;
                    $$.quad_op_type = QO_TEMP;
                    $$.type = INT;
                    $$.temp.symbol_type = ATOMIC_TYPE;
                    $$.temp.ptr_to_index = 0;
                    $$.temp.ptr = newtemp(0);
                    gencode (get_instr($2, 0), $1, $4, $$);
                }
                else if ($2 == OPB_L || $2 == OPB_L_EQ || $2 == OPB_G ||
                        $2 == OPB_G_EQ || $2 == OPB_EQ || $2 == OPB_DIFF) {
                    $$.type = BOOL;
                    $$.is_array = 0;
                    $$.true = create_quad_list(quad_table.nextquad);
                    gencode(get_instr($2, 0), $1, $4, $$);
                    $$.false = create_quad_list(quad_table.nextquad);
                    gencode(GOTO_QUAD, $$, $$, $$);
                }
            }
            else {
                struct quad_list_t *temp_1_false;
                struct quad_list_t *temp_2_false;
                struct quad_list_t *temp_1_true;
                struct quad_list_t *temp_2_true;

                struct quad_list_t *not_1_false;
                struct quad_list_t *not_2_false;
                struct quad_list_t *not_1_true;
                struct quad_list_t *not_2_true;
                
                int n;
                $$.is_array = 0;
                $$.type = BOOL;
                switch($2) {
                case OPB_AND    :
                    $$.false = concat_quad_list($1.false, $4.false);
                    complete_quad_list($1.true, $3);
                    free_quad_list($1.true);
                    $$.true = $4.true;
                break;
                case OPB_OR     :
                    $$.true = concat_quad_list($1.true, $4.true);
                    complete_quad_list($1.false, $3);
                    free_quad_list($1.false);
                    $$.false = $4.false;
                break;
                case OPB_EQ     :
                case OPB_DIFF   :
                case OPB_XOR    :
                    temp_1_true = concat_quad_list($1.true, $4.true);
                    complete_quad_list($1.false, $3);
                    temp_1_false = $4.false;
                    complete_quad_list(temp_1_true, quad_table.nextquad);

                    not_1_true = create_quad_list(quad_table.nextquad);
                    gencode(quad_table.quads[$1.true->position].instruction, 
                            quad_table.quads[$1.true->position].op1, 
                            quad_table.quads[$1.true->position].op2, 
                            quad_table.quads[$1.true->position].res);
                    
                    not_1_false = create_quad_list(quad_table.nextquad);
                    gencode(quad_table.quads[$1.false->position].instruction, 
                            quad_table.quads[$1.false->position].op1, 
                            quad_table.quads[$1.false->position].op2, 
                            quad_table.quads[$1.false->position].res);
                    
                    n = quad_table.nextquad;
                    not_2_true = create_quad_list(quad_table.nextquad);
                    gencode(quad_table.quads[$4.true->position].instruction, 
                            quad_table.quads[$4.true->position].op1, 
                            quad_table.quads[$4.true->position].op2, 
                            quad_table.quads[$4.true->position].res);
                    
                    not_2_false = create_quad_list(quad_table.nextquad);
                    gencode(quad_table.quads[$4.false->position].instruction, 
                            quad_table.quads[$4.false->position].op1, 
                            quad_table.quads[$4.false->position].op2, 
                            quad_table.quads[$4.false->position].res);
                    
                    temp_2_true = concat_quad_list(not_1_false, not_2_false);
                    complete_quad_list(not_1_true, n);
                    free_quad_list(not_1_true);
                    temp_2_false = not_2_true;

                    if ($2 == OPB_EQ) {
                        $$.true = concat_quad_list(temp_1_false, temp_2_false);
                        $$.false = temp_2_true;
                    }
                    else {
                        $$.false = concat_quad_list(temp_1_false, temp_2_false);
                        $$.true = temp_2_true;
                    }
                    free_quad_list($1.false);
                    free_quad_list(temp_1_true);
                break;
                }
            }
        }
        }
    | opu expr %prec OPU {
        if ($2.type == STRING) {
            handle_error("No operation allowed for type string");
        }
        if ($2.is_array) {
            handle_error("No operation allowed for arrays");
        }
        if ($2.type == INT && $1 != OP_MINUS) {
            handle_error("unary operator [not] forbidden for int");
        }
        else if ($2.type == BOOL && $1 != OPU_NOT) {
            handle_error("unary operator [-] forbidden for bool");
        }
        if ($2.quad_op_type == QO_CST && $2.type != BOOL) {
            $$ = compute_opu_const_expr($2, $1);
        }
        else {
            if ($2.type == INT) {
                $$.is_array = 0;
                $$.quad_op_type = QO_TEMP;
                $$.type = $2.type;
                $$.temp.symbol_type = ATOMIC_TYPE;
                $$.temp.ptr_to_index = 0;
                $$.temp.ptr = newtemp(0);
                gencode (get_instr($1, 1), $2, $2, $$);
            }
            else {
                $$.is_array = 0;
                $$.quad_op_type = QO_TEMP;
                $$.type = $2.type;
                $$.true  = $2.false;
                $$.false = $2.true;
            }
        }
        }
    | IDENT '(' exprlist ')' {
        int ptr = is_symbol_in_table($1, 0);
        if (ptr == -1) {
            handle_error("function [%s] is not declared", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[ptr];
        if (cur_symbol.var_func_par != FUNC_T) {
            handle_error("invalide use of [%s], [%s] isn't a function", $1, $1);
        }
        if (cur_symbol.type.func.atomic_type == VOID_A) {
            handle_error("function [%s] is of unit type. Can't be called "
                "inside an expression.", $1);
        }
        int nb_parameters = list_len($3);
        if (cur_symbol.type.func.nb_param != nb_parameters) {
            handle_error("arguments given in call of function [%s] are "
                "different from [%s] type", $1, $1);
        }
        for (int i = 0; i < nb_parameters; i++) {
            struct expr_t *expr_temp = (struct expr_t *)list_get_first($3);
            struct param_t cur_param = 
                symbol_table.symbols[ptr + 1 + i].type.param;
            if (cur_param.typename->atomic_type != expr_temp->type) {
                handle_error("arguments given in call of function [%s] are "
                    "different from [%s] type, for parameter [%i]", $1, $1,i+1);
            }
            if (cur_param.typename->symbol_type == ARRAY_TYPE) {
                if (cur_param.typename->symbol_type == ARRAY_TYPE && 
                    !expr_temp->is_array) {
                    handle_error("arguments given in call of function [%s] are "
                        "different from [%s] type, for parameter [%i]", 
                        $1, $1,i+1);
                }
                struct typename_t *arraytype_2;
                switch (symbol_table.symbols[expr_temp->index_symbol_table]
                        .var_func_par) {
                case VAR_T:
                    arraytype_2 = 
                        symbol_table.symbols[expr_temp->index_symbol_table]
                        .type.var.typename;
                    break;
                case PARAM_T:
                    arraytype_2 = 
                        symbol_table.symbols[expr_temp->index_symbol_table]
                        .type.param.typename;
                    break;
                default:
                    handle_error("arguments given in call of function [%s] are "
                    "different from [%s] type, for parameter [%i]", $1, $1,i+1);
                    break;
                }
                if (cur_param.typename->len_range_list != 
                    arraytype_2->len_range_list) {
                    handle_error("arguments given in call of function [%s] are "
                    "different from [%s] type, for parameter [%i]", $1, $1,i+1);
                }
                for (int i = 0; i < arraytype_2->len_range_list; i++) {
                    if ((arraytype_2->range_array[i][0] != 
                        cur_param.typename->range_array[i][0]) ||
                        (arraytype_2->range_array[i][1] != 
                        cur_param.typename->range_array[i][1])) {
                        handle_error("arguments given in call of function [%s]"
                            "are different from [%s] type, for parameter [%i]", 
                            $1, $1,i+1);
                    }
                }
            }
            gencode(PARAM_QUAD, *expr_temp, *expr_temp, *expr_temp);
            list_pop($3);
        }
        list_free($3);
        $$.is_array = 0;
        $$.quad_op_type = QO_TEMP;
        $$.type = cur_symbol.type.func.atomic_type;
        $$.temp.symbol_type = ATOMIC_TYPE;
        $$.temp.ptr_to_index = 0;
        $$.temp.ptr = newtemp(0);

        struct expr_t expr_func;
        expr_func.quad_op_type = QO_VAR;
        expr_func.type = cur_symbol.type.func.atomic_type;
        expr_func.var.ptr = ptr;
        expr_func.var.symbol_type = ATOMIC_TYPE;
        expr_func.var.ptr_to_index = 0;

        struct expr_t result;
        result.is_array = 0;
        result.type = INT;
        result.quad_op_type = QO_CST;
        result.const_int = nb_parameters;
        gencode(CALL_AFF_QUAD, $$, expr_func, result);
        if ($$.type == BOOL) {
            $$.true = create_quad_list(quad_table.nextquad);
            gencode(IF_QUAD, $$, $$, $$);
            $$.false = create_quad_list(quad_table.nextquad);
            gencode(GOTO_QUAD, $$, $$, $$);
        }
        free($1);
        }
    | IDENT '(' ')' {
        int ptr = is_symbol_in_table($1, 0);
        if (ptr == -1) {
            handle_error("function [%s] is not declared", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[ptr];
        if (cur_symbol.var_func_par != FUNC_T) {
            handle_error("invalide use of [%s], [%s] isn't a function", $1, $1);
        }
        if (cur_symbol.type.func.atomic_type == VOID_A) {
            handle_error("function [%s] is of unit type. Can't be called "
                "inside an expression.", $1);
        }
        int nb_parameters = 0;
        if (cur_symbol.type.func.nb_param != nb_parameters) {
            handle_error("arguments given in call of function [%s] are "
                "different from [%s] type", $1, $1);
        }
        $$.is_array = 0;
        $$.quad_op_type = QO_TEMP;
        $$.type = cur_symbol.type.func.atomic_type;
        $$.temp.symbol_type = ATOMIC_TYPE;
        $$.temp.ptr_to_index = 0;
        $$.temp.ptr = newtemp(0);

        struct expr_t expr_func;
        expr_func.quad_op_type = QO_VAR;
        expr_func.type = $$.type;
        expr_func.var.ptr = ptr;
        expr_func.var.symbol_type = ATOMIC_TYPE;
        expr_func.var.ptr_to_index = 0;

        struct expr_t result;
        result.type = INT;
        result.quad_op_type = QO_CST;
        result.const_int = nb_parameters;
        gencode(CALL_AFF_QUAD, $$, expr_func, result);
        if ($$.type == BOOL) {
            $$.true = create_quad_list(quad_table.nextquad);
            gencode(IF_QUAD, $$, $$, $$);
            $$.false = create_quad_list(quad_table.nextquad);
            gencode(GOTO_QUAD, $$, $$, $$);
        }
        free($1);
        }
    | IDENT '[' exprlist ']' {
        int scope = symbol_table.cur_symbol_scope;
        int ptr, temp_ptr;
        if ((ptr = is_symbol_in_table($1, scope)) == -1) {
            handle_error("variable [%s] is not declared in this scope", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[ptr];
        if (cur_symbol.var_func_par == FUNC_T) {
            handle_error("invalide use of [%s], [%s] is a function", $1, $1);
        }

        struct expr_t array_ident;
        array_ident.var.ptr = ptr;
        array_ident.var.symbol_type = ATOMIC_TYPE;
        array_ident.var.ptr_to_index = 0;
        array_ident.quad_op_type = QO_VAR;
        if (cur_symbol.var_func_par == PARAM_T) {
            array_ident.type = cur_symbol.type.param.typename->atomic_type;
        }
        else {
            array_ident.type = cur_symbol.type.var.typename->atomic_type;
        }
        array_ident.is_array = 1;
        array_ident.index_symbol_table = ptr;
        array_ident.true = NULL;
        array_ident.false = NULL;

        struct expr_t array_expr;
        array_expr.temp.ptr = newtemp(0);
        array_expr.quad_op_type = QO_TEMP;
        array_expr.temp.symbol_type = ARRAY_TYPE;
        array_expr.is_array = 1;
        array_expr.index_symbol_table = ptr;
        array_expr.type = array_ident.type;
        gencode (AFF_QUAD, array_ident, array_ident, array_expr);

        array_expr.is_array = 0;
        if (cur_symbol.var_func_par == PARAM_T) {
            array_expr.type = cur_symbol.type.param.typename->atomic_type;
            array_expr.temp.ptr_to_index = get_index_array (
                cur_symbol.type.param.typename->range_array,
                cur_symbol.type.param.typename->len_range_list,
                $3);
        }
        else {
            array_expr.type = cur_symbol.type.var.typename->atomic_type;
            array_expr.temp.ptr_to_index = get_index_array (
                cur_symbol.type.var.typename->range_array,
                cur_symbol.type.var.typename->len_range_list,
                $3);
        }
        $$.is_array = 0;
        $$.quad_op_type = QO_TEMP;
        $$.type = array_expr.type;
        $$.temp.symbol_type = ATOMIC_TYPE;
        $$.temp.ptr_to_index = 0;
        $$.temp.ptr = newtemp(0);
        gencode (AFF_QUAD, array_expr, array_expr, $$);
        if ($$.type == BOOL) {
            $$.true = create_quad_list(quad_table.nextquad);
            gencode(IF_QUAD, $$, $$, $$);
            $$.false = create_quad_list(quad_table.nextquad);
            gencode(GOTO_QUAD, $$, $$, $$);
        }
        list_free($3);
        free($1);
        }
    | IDENT {
        struct expr_t cur_var;
        int scope = symbol_table.cur_symbol_scope;
        if ((cur_var.var.ptr = is_symbol_in_table($1, scope)) == -1) {
            handle_error("variable [%s] is not declared in this scope", $1);
        }
        struct symbol_t cur_symbol = symbol_table.symbols[cur_var.var.ptr];
        if (cur_symbol.var_func_par == FUNC_T) {
            handle_error("invalide use of [%s], [%s] is a function", $1, $1);
        }
        if (cur_symbol.var_func_par == PARAM_T && 
            cur_symbol.type.param.typename->symbol_type == ARRAY_TYPE) {
            cur_var.is_array = 1;
        }
        else if (cur_symbol.var_func_par == VAR_T && 
            cur_symbol.type.var.typename->symbol_type == ARRAY_TYPE) {
            cur_var.is_array = 1;
        }
        else {
            cur_var.is_array = 0;
        }
        cur_var.quad_op_type = QO_VAR;
        if (cur_symbol.var_func_par == PARAM_T) {
            cur_var.type = cur_symbol.type.param.typename->atomic_type;
            cur_var.var.symbol_type = 
                cur_symbol.type.param.typename->symbol_type;
        }
        else {
            cur_var.type = cur_symbol.type.var.typename->atomic_type;
            cur_var.var.symbol_type = cur_symbol.type.var.typename->symbol_type;
        }
        cur_var.var.ptr_to_index = 0;
        if (cur_var.type == BOOL) {
            cur_var.true = create_quad_list(quad_table.nextquad+1);
            cur_var.false = create_quad_list(quad_table.nextquad+2);
        }
        else {
            cur_var.false = NULL;
            cur_var.true = NULL;
        }
        free($1);

        $$.is_array = cur_var.is_array;
        $$.type = cur_var.type;
        $$.quad_op_type = QO_TEMP;
        $$.false = cur_var.false;
        $$.true = cur_var.true;
        $$.temp.symbol_type = cur_var.var.symbol_type;
        $$.temp.ptr_to_index = cur_var.var.symbol_type;
        $$.temp.ptr = newtemp(0);
        if ($$.is_array) {
            $$.index_symbol_table = cur_var.var.ptr;
        }
        gencode (AFF_QUAD, cur_var, cur_var, $$);
        if (cur_var.type == BOOL) {
            gencode(IF_QUAD, $$, $$, $$);
            gencode(GOTO_QUAD, $$, $$, $$);
        }
        }

opb : 
      OPB_PLUS   {$$ = OPB_PLUS;}
    | OP_MINUS   {$$ = OP_MINUS;}
    | OPB_STAR   {$$ = OPB_STAR;}
    | OPB_DIVIDE {$$ = OPB_DIVIDE;}
    | OPB_POW    {$$ = OPB_POW;}
    | OPB_L      {$$ = OPB_L;}
    | OPB_L_EQ   {$$ = OPB_L_EQ;}
    | OPB_G      {$$ = OPB_G;}
    | OPB_G_EQ   {$$ = OPB_G_EQ;}
    | OPB_EQ     {$$ = OPB_EQ;}
    | OPB_DIFF   {$$ = OPB_DIFF;}
    | OPB_AND    {$$ = OPB_AND;}
    | OPB_OR     {$$ = OPB_OR;}
    | OPB_XOR    {$$ = OPB_XOR;}

opu : 
      OP_MINUS   {$$ = OP_MINUS;}
    | OPU_NOT    {$$ = OPU_NOT;}

mark : {$$ = quad_table.nextquad;}

%%

/* -------------------------------------------------------------------------- */

void yyerror(const char * msg) {
    fprintf(stderr, "%s at line [%i].\n", msg, current_line);
    exit(EXIT_FAILURE);
}

noreturn void handle_error(const char * msg, ...) {
    va_list ap;

    va_start(ap, msg);
    vfprintf(stderr, "error : ", ap);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    fprintf(stderr, "Compiler stop at line [%i].\n", current_line);

    exit(EXIT_FAILURE);
}

noreturn void handle_perror(const char * msg, ...) {
    va_list ap;

    va_start(ap, msg);
    vfprintf(stderr, "perror : ", ap);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    perror("");
    fprintf(stderr, "Compiler stop at line [%i].\n", current_line);

    exit(EXIT_FAILURE);
}

void check_snprintf(int result, int wsize) {
    if ((result) < 0 || (result) >= wsize) {
        handle_perror("snprintf failed\n");
    }
}

/* -------------------------------------------------------------------------- */

int main (int argc, char * argv[]) {
    args_t args;
    parse_args(argc, argv, &args);

    if (args.flags & VERSION) {
        display_version();
    }
    if (!(args.flags & OUT_FILE)) {
        printf("\nno output file specified, default output file used : "
            "<output.s>\n\n");
        CHECK(args.fd = open ("output.s", O_WRONLY | O_CREAT | O_TRUNC, 0666));
    }
    fd_out = args.fd;

    init_symbol_table();
    init_quad_table();

    int c = yyparse();
    
    if (args.flags & SYM_TABLE) {
        display_symbol_table();
    }
    if (DEBUG) {
        display_quad_table();
    }

    CHECK(close(args.fd));
    free(string_table.strings);
    free_quad_table();
    free_symbol_table();
    yylex_destroy();
    return c;
}

/* TODO LIST

IMPORTANT
 - TODO error if temp >= 8 -> handle_error()
 - TODO option to display quad symbol (like -tos)
 - TODO if then else -> reduce reduce conflict -> A
 - TODO split files -> ?
 - TODO put grammar "instruction" in function in a other file -S ?
 - TODO do an error test for each handle_error -> S
 - TODO documenatation and test files -> S

OPTIONAL

 - TODO ref param
 - TODO in gencode argument should be of type expr_t * and accept NULL pointers
 - TODO verif xor with subject 2019-2020

Q : rename linked_list by linked_list_t ?
Q : (2^3)*9 != 2^3*9 priority ?
Q : src include dir ?
Q : cste string regular expression bug for  "//" valid, "/" not valid,
    add single quote example " '"' " is a valid syntaxe
Q : getopt.h for args ?
Q : test if output file is part of the source code ?
Q : hash table ?

verif :
 - yacc warning
 - valgrind
 - malloc check
 - system call check
 - remove useless command and test
 - -Wall -Werror -Wextra

Do a real README file

*/
