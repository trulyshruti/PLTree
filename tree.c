#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tree.h"


void print(struct tree *str) {
	int width = str->width;
	int i = 0;
	while (i < width) {
 		putchar(*(char *)(get_branch(str, i)->data)); 
		i++; 
	}
 }

void init_tree(struct tree *root) {
	root->data = NULL;
	root->children = NULL;
	root->sibling = NULL;
	root->width = 0;
}

struct tree *treemake(char *str) {
	struct tree *root = malloc(sizeof (struct tree));
	int len = strlen(str);
	int i;

	init_tree(root);
	
	for (i = 0; i <= len; i++) {
		struct tree *child = malloc(sizeof(struct tree));
		char *data = malloc(sizeof(char));
		*data = str[i];
		init_tree(child);
		child->data = data;

		add_child(root, child);
	}


	return root;
}

/*
struct tree *treemake(int i, struct tree *first, ...) {
	struct tree *root = malloc(sizeof(struct tree));
	init_tree(root);

	*(int *)(root->data) = i;
	
	while (*children != NULL) {
		struct tree *child = *children;
		add_child(root, child);
		children++;
	}

	return root;
}
*/

int add_sibling (struct tree *root, struct tree *sibling, int n) {
	if (root == NULL)
		return -1;
	if (root->sibling == NULL) {
		root->sibling = sibling;
		return n;
	} else {
		return add_sibling(root->sibling, sibling, n+1);
	}

}

int add_child (struct tree *root, struct tree *child) {
	if (root == NULL) {
		return -1;
	}
	if (root->children == NULL) {
		root->children = child;
		root->width = 1;
		return 1;
	} else {
		int n = add_sibling(root->children, child, 1);
		root->width = n;
		return n;
	}
}

void set_type (struct tree *root, data_type t) {
	root->type = t;
}

data_type get_type(struct tree *t) {
	return t->type;
}

struct tree *get_ith_sibling(struct tree *root, int i) {
	if (i == 0) {
		return root;
	}

	if (root->sibling == NULL) {
		return NULL;
	}

	return get_ith_sibling(root->sibling, i-1);
}

struct tree *get_branch(struct tree *root, int branch) {
	return get_ith_sibling(root->children, branch);
}
