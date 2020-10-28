#ifndef SCALPA_H
#define SCALPA_H

#include <stdnoreturn.h>
#include <stdarg.h>
#include "linked-list.h"

#define INIT_TABLE_SIZE 1024

enum type_t {INT, STRING, BOOL};
enum atomic_type_t {INT_A, BOOL_A, VOID_A};
enum symbol_type_t {ATOMIC_TYPE, ARRAY_TYPE};

/* 
 * Print the error message in the terminal and
 * exit program with EXIT_FAILURE
 */
noreturn void handle_error(const char * msg, ...);

struct cste_value_t {
    enum type_t type;
    union {
        int iconst;   // int
        char *sconst; // string
        int bconst;   // bool
    } val;
};

struct typename_t {
    enum atomic_type_t atomic_type;
    enum symbol_type_t symbol_type;
    struct linked_list *rangelist;
};

struct symbol_t {
    char * ident; // name of the variable
    int ident_length; //length of the identifier name
    int initialiazed; // 0 if only declared, 1 if a value as been affected
    enum atomic_type_t atomic_type;
    enum symbol_type_t symbol_type;
    //struct linked_list *rangelist;
    int len_range_list;
    int (*rangelist)[2];
};

struct symbol_table_t {
    int table_size;
    int last_ident_index;
    struct symbol_t *symbols;
};

#endif