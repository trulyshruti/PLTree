#ifndef __LL_H__
#define __LL_H__
struct List {
	struct node *head;
	struct node *tail;
};

struct node {
	void *data;
	struct node *next;
};

void init_list(struct List *list);

void traverse_list(struct List *list, void (*f)(void *));

void free_list(struct List *list);

void *get(struct List *list, int i);

void append(struct List *list, void *data);

#endif
