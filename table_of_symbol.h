#ifndef TABLE_OF_SYMBOL_H
#define TABLE_OF_SYMBOL_H

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

/**
 * @brief display a symbol
 * display depends on type of the symbol (function, parameter, variable)
 * @param symbol
 * @param index
 * @param n number of digit in max index, used to align every columns
 */
void display_symbol(struct symbol_t *symbol, int index, int n);

/**
 * @brief display symbol table
 * symbol table need to be initialized before calling this function
 */
void display_symbol_table();

/**
 * @brief check if an identifier already exist in the table for a given scope
 * @param identname, name of the symbol
 * @param scope, scope of the symbol
 * @result 1 if symbol is in table, else 0
 */
int is_symbol_in_table(char *identname, int scope);

/**
 * @brief initialize a pointer of type fundecl_t * (create a malloc)
 * @param _ident
 * @param _atomictype
 * @param _parlist
 * @param _vardecllist
 * @result allocated struct fundecl_t * pointer
 */
struct fundecl_t * create_fundecl(char *_ident, 
                                  int _atomictype,
                                  struct linked_list *_parlist,
                                  struct linked_list *_vardecllist);

/**
 * @brief initialize a pointer of type vardecl_t * (create a malloc)
 * @param _identlist
 * @param _typename
 * @result allocated struct vardecl_t * pointer
 */
struct vardecl_t * create_vardecl(struct linked_list *_identlist,
                                  struct typename_t *_typename);

/**
 * @brief initialize a pointer of type typename_t * (create a malloc)
 * for an array type symbol (variable or parameter)
 * @param _range_list
 * @param _atomic_type
 * @result allocated struct typename_t * pointer
 */
struct typename_t * create_typename_array(struct linked_list *_range_list,
                                          int _atomic_type);

/**
 * @brief initialize a pointer of type typename_t * (create a malloc)
 * for an atomic type symbol (variable or parameter)
 * @param _atomic_type
 * @result allocated struct typename_t * pointer
 */
struct typename_t * create_typename_atomic(int _atomic_type);

/**
 * @brief add INIT_TABLE_SIZE allocated space to the current table 
 * for new symbols. realloc only if the table is full else do nothing
 */
void realloc_table ();

/**
 * @brief copy an element of type typename_t from origin to destination
 * @param dest, pointer to destination (create a malloc need to be free later)
 * @param origin, typename to copy
 */
void copy_typename_table(struct typename_t *dest, struct typename_t origin);

/**
 * @brief add multiple variable in the symbol table
 * var var1, var2, var3 : type;
 * @param identlist, list of symbols to add to the table
 * @param _typename, type of symbols to add to the table
 */
void add_vardecl_table(struct linked_list *identlist,
                       struct typename_t *_typename);

/**
 * @brief add multiple variable declaration in the symbol table
 * @param vardecllist, list of var declaration to add to the table
 * each var declaration is a list of variable.
 */
void add_vardecllist_table(struct linked_list *vardecllist);

/**
 * @brief add multiple parameters in the symbol table
 * @param parlist, list of parameters to add to the table
 */
void add_paramlist_table(struct linked_list *parlist);

/**
 * @brief add multiple function in the symbol table
 * @param fundecllist, list of functions to add to the table
 */
void add_fundecllist_table(struct linked_list *fundecllist);

#endif