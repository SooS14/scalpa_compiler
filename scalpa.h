#ifndef SCALPA_H
#define SCALPA_H

#include <stdnoreturn.h>
#include <stdarg.h>
#include "linked_list.h"

enum type_t {INT, STRING, BOOL};
enum atomic_type_t {INT_A, BOOL_A, VOID_A};
enum symbol_type_t {ATOMIC_TYPE, ARRAY_TYPE, FUNCTION_TYPE, PARAMETER_TYPE};
enum var_func_par_t {VAR_T, FUNC_T, PARAM_T}; // find an other name later

/**
 * @brief Print an error message in stderr and exit program with EXIT_FAILURE
 * @param msg message to print in stderr
 */
noreturn void handle_error(const char * msg, ...);

struct cste_value_t {
    enum type_t type;
    union {
        // int
        int iconst;
        // string
        char *sconst;
        // bool
        int bconst;
    } val;
};

struct typename_t {
    // atomic type : int / bool / unit
    enum atomic_type_t atomic_type;
    // symbol type : atomic_type / array_type
    enum symbol_type_t symbol_type;
    // rangelist if array type, not initialized if atomic type
    struct linked_list *range_list;
    // size of range list, 0 if atomic type
    int len_range_list;
    // rangelist if array type, NULL if atomic type
    int (*range_array)[2];
};

struct param_t {
    char * ident; // used for parlist
    struct typename_t typename;
    int ref;  // bool indicating if it's a ref
};

struct function_t {
    int nb_param; // number of paramater for a function
    int *index_param; // array of index of parameter of a function
    enum atomic_type_t atomic_type; // atomic type : int / bool / unit
};

struct variable_t {
    struct typename_t typename;
    int initialiazed;
};

struct symbol_t {
    // name of the variable
    char * ident;
    //length of the identifier name
    int ident_length;
    // 0 if declared, 1 if a value as been affected
    enum var_func_par_t var_func_par;
    union {
        struct param_t param;
        struct function_t func;
        struct variable_t var;
    } type;
    // scope of the symbol
    // 0 -> global variable
    // i > 0 -> local variable, i represent the index of the function it 
    // depends on (in the symbol table)
    // for function scope = 0
    int scope;
};

struct symbol_table_t {
    // size of symbol table (allocated)
    int table_size;
    // number of symbols in the table
    int last_ident_index;
    // array of symbols
    struct symbol_t *symbols;
    // indicate the scope of the current symbol, starting at 0 and incremented
    // each time a new function is declared, cur_symbol_scope take the value of 
    // the index of the function
    int cur_symbol_scope;
};

#endif