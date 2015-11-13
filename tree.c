#include <stdio.h>
#include "tree.h"


void init_tree(struct tree *root) {
	root->data = NULL;
	root->children = NULL;
	root->sibling = NULL;
	root->width = 0;
}


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
		int n = add_sibling(root->children, child, root->width);
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
