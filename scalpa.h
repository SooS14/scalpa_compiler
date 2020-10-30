#ifndef SCALPA_H
#define SCALPA_H

#include <stdnoreturn.h>
#include <stdarg.h>
#include "linked_list.h"

enum type_t {INT, STRING, BOOL};
enum atomic_type_t {INT_A, BOOL_A, VOID_A};
enum symbol_type_t {ATOMIC_TYPE, ARRAY_TYPE};

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
    // size of range list, 0 if atomic type
    enum symbol_type_t symbol_type;
    // rangelist if array type, not initialized if atomic type
    struct linked_list *rangelist;
};

struct symbol_t {
    // name of the variable
    char * ident;
    //length of the identifier name
    int ident_length;
    // 0 if declared, 1 if a value as been affected
    int initialiazed;
    // atomic type : int / bool / unit
    enum atomic_type_t atomic_type;
    // symbol type : atomic_type / array_type
    enum symbol_type_t symbol_type;
    // size of range list, 0 if atomic type
    int len_range_list;
    // rangelist if array type, NULL if atomic type
    int (*rangelist)[2];
};

struct symbol_table_t {
    // size of symbol table (allocated)
    int table_size;
    // number of symbols in the table
    int last_ident_index;
    // array of symbols
    struct symbol_t *symbols;
};

#endif