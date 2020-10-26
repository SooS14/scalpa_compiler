#ifndef SCALPA_H
#define SCALPA_H

#include <stdnoreturn.h>
#include <stdarg.h>

/* 
 * Print the error message in the terminal and
 * exit program with EXIT_FAILURE
 */
noreturn void handle_error(const char * msg, ...);

enum type_t {INT, STRING, BOOL};
enum atomic_type_name_t {INT_A, BOOL_A, VOID_A};

struct cste_value_t {
    enum type_t type;
    union {
        int iconst;   // int
        char *sconst; // string
        int bconst;   // bool
    } val;
};

struct atomic_type_t {
    char * identifier; // name of the variable
    int identifier_length; //length of the identifier name
    enum atomic_type_name_t type;
    int initialiazed; // 0 if only declared, 1 if a value as been affected
    union {
        int integer;   // int
        int boolean;   // bool
        // no value for void type
    } value;
};

#endif