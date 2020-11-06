#ifndef VAR_DECLARATION_H
#define VAR_DECLARATION_H

#define INIT_TABLE_SIZE 1024

/**
 * @brief initialize the symbol table
 * call free_symbol_table() to free allocated memory later
 */
void init_symbol_table();

/**
 * @brief free all memory allocated for the symbol table and its element
 */
void free_symbol_table();

void display_symbol(struct symbol_t *symbol, int index, int n);

/**
 * @brief display symbol table
 * symbol table need to be initialized before calling this function
 */
void display_symbol_table();

// changes : scope as been added
/**
 * @brief check if the name of the symbol is in table
 * @param char * identname, name of the symbol
 * @result 1 if symbol is in table, else 0
 */
int is_symbol_in_table(char *identname, int scope);

struct fundecl_t * create_fundecl(char *_ident, 
                                  int _atomictype,
                                  struct linked_list *_parlist,
                                  struct linked_list *_vardecllist);

struct vardecl_t * create_vardecl(struct linked_list *_identlist,
                                  struct typename_t *_typename);

struct typename_t * create_typename_array(struct linked_list *_range_list,
                                          int _atomic_type);

struct typename_t * create_typename_atomic(int _atomic_type);

/**
 * @brief add INIT_TABLE_SIZE allocated space to the current table 
 * for new symbols. This function is only called by add_symbol_list()
 */
void realloc_table ();

void copy_typename_table(struct typename_t *dest, struct typename_t origin);

/**
 * @brief add multiple symbols in the symbol table
 * @param struct linked_list *identlist, list of symbols to add to the table
 * @param struct typename_t, type of symbols to add to the table,
 * contain arraytype / atomictype, rangelist if arraytype
 * /!\ only use for variable declaration (not function or parameter)
 */
void add_var_symbol_list(struct linked_list *identlist, struct typename_t type);

void add_vardecl_table(struct linked_list *identlist,
                       struct typename_t *_typename);

void add_vardecllist_table(struct linked_list *vardecllist);

void add_paramlist_table(struct linked_list *parlist);

void add_vardecllist_table(struct linked_list *vardecllist);

void add_fundecllist_table(struct linked_list *fundecllist);

#endif