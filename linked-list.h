#ifndef LINKED_LIST_H
#define LINKED_LIST_H

struct node {
  void *data;
  struct node * next;
};

struct linked_list {
  int length;
  struct node * first;
};

struct linked_list * list_init(void);

void list_free(struct linked_list *list);

void list_push(struct linked_list *list, void *x);

void * list_pop(struct linked_list *list);

int len_list(struct linked_list *list);

#endif