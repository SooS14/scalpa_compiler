#ifndef SCALPA_H
#define SCALPA_H

enum type_t {INT, STRING, BOOL};

struct cste_value_t{
    enum type_t type;
    union {
        int iconst;   // int
        char *sconst; // string
        int bconst;   // bool
    } val;
};

#endif