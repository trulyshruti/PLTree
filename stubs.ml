let tree_c = 
"
typedef enum {INT, CHAR, DOUBLE, BOOL, VOID, TREE} data_type;

union data_u {
	int i;
	char c;
	double d;
	struct tree *t;
};

struct tree {
	data_type type;
	union data_u data;
	int width;
	int refcount;
	struct List *children;
};

void print (struct tree *str);

void init_tree (struct tree *root);

void free_tree(struct tree *t);

struct tree* inc_refcount(struct tree *t);
struct tree* dec_refcount(struct tree *t);

int equal(struct tree *lhs, struct tree *rhs);
int nequal(struct tree *lhs, struct tree *rhs);
int lt(struct tree *lhs, struct tree *rhs);
int gt(struct tree *lhs, struct tree *rhs);
int lte(struct tree *lhs, struct tree *rhs);
int gte(struct tree *lhs, struct tree *rhs);

struct tree *sub(struct tree *lhs, struct tree *rhs);
struct tree *add(struct tree *lhs, struct tree *rhs);
struct tree *mult(struct tree *lhs, struct tree *rhs);
struct tree *divd(struct tree *lhs, struct tree *rhs);
struct tree *mod(struct tree *lhs, struct tree *rhs);

struct tree *int_treemake(int i_data, struct tree *child, ...);
struct tree *char_treemake(char c_data, struct tree *child, ...);
struct tree *double_treemake(int d_data, struct tree *child, ...);
struct tree *tree_treemake(struct tree *t_data, struct tree *child, ...);
struct tree *void_treemake(struct tree *child, ...);

struct tree *treemake(data_type type, union data_u data, struct tree *child, va_list args);
int add_sibling (struct tree *root, struct tree *sibling, int n);
int add_child (struct tree *root, struct tree *child);
void set_type (struct tree *root, data_type type);
data_type get_type(struct tree *root);
struct tree *get_ith_sibling(struct tree *root, int i);
struct tree *get_branch_t(struct tree *root, struct tree *branch);
struct tree *get_branch(struct tree *root, int branch);


void print(struct tree *str) {
	int i = 0;
	int len;

	if (str == NULL)
		return;

	len = str->width;
	switch(str->type) {
		case CHAR:
			putchar(str->data.c);
			break;
		case INT:
			printf(\"%d\", str->data.i);
			break;
		case DOUBLE:
			printf(\"%f\", str->data.d);
			break;
		case TREE:
			print(str->data.t);
			break;
		default:
			break;
	}

	while (i < len) {	
		print(get_branch(str, i++));
	}

}


int equal(struct tree *lhs, struct tree *rhs) {

	int retval;
	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = 0;
	} else {	
		switch(lhs->type) {
			case CHAR:
				retval = lhs->data.c == rhs->data.c;
				break;
			case INT:
				retval = lhs->data.i == rhs->data.i;
				break;
			case DOUBLE:
				retval = lhs->data.d == rhs->data.d;
				break;
			default:
				retval = 0;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);
	return retval;
}

int nequal(struct tree *lhs, struct tree *rhs) {
	return !equal(lhs, rhs);
}
int lt(struct tree *lhs, struct tree *rhs) {
	int retval;
	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		fprintf(stderr, \"TYPE MISMATCH!\\n\");
		retval = 0;
	} else {	
		switch(lhs->type) {
			case CHAR:
				retval = lhs->data.c < rhs->data.c;
				break;
			case INT:
				retval = lhs->data.i < rhs->data.i;
				break;
			case DOUBLE:
				retval = lhs->data.d < rhs->data.d;
				break;
			default:
				retval = 0;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);
	return retval;
}
int gt(struct tree *lhs, struct tree *rhs) {
	int retval;
	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = 0;
	} else {	
		switch(lhs->type) {
			case CHAR:
				retval = lhs->data.c > rhs->data.c;
				break;
			case INT:
				retval = lhs->data.i > rhs->data.i;
				break;
			case DOUBLE:
				retval = lhs->data.d > rhs->data.d;
				break;
			default:
				retval = 0;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);
	return retval;
}
int lte(struct tree *lhs, struct tree *rhs) {
	return gt(rhs, lhs);
}
int gte(struct tree *lhs, struct tree *rhs) {
	return lt(rhs, lhs);
}
void dec_refcount_child(void *child) {
	dec_refcount((struct tree *)child);
}

void free_tree(struct tree *t) {
	if (t == NULL) {
		return;
	}

	traverse_list(t->children, dec_refcount_child);
	free_list(t->children);
	if (t->type == TREE) {
		dec_refcount(t->data.t);
	}
	free(t);
}



struct tree* inc_refcount(struct tree *t) {
	if (t) {
		t->refcount++;
	}

	return t;
}

struct tree *dec_refcount(struct tree *t) {
	if (t) {
		if (--(t->refcount) <= 0) {
			free_tree(t);
			t = NULL;
		}
	}

	return t;
}

struct tree *sub(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c - rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i - rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d - rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;
}
struct tree *add(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c + rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i + rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d + rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;
}

struct tree *mult(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c * rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i * rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d * rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;

}

struct tree *divd(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c / rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i / rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d / rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;
}

struct tree *mod(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c % rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i % rhs->data.i, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;
}
void init_tree(struct tree *root) {
	root->children = malloc(sizeof(struct List));
	root->type = VOID;
	root->width = 0;
	root->refcount = 0;
	init_list(root->children);
}

struct tree *int_treemake(int i_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.i = i_data;
	root = treemake(INT, data, child, args);
	va_end(args);


	return root;
}
	

struct tree *char_treemake(char c_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.c = c_data;
	root = treemake(CHAR, data, child, args);
	va_end(args);

	return root;
}

	
struct tree *double_treemake(int d_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.d = d_data;
	root = treemake(DOUBLE, data, child, args);
	va_end(args);

	return root;
}

struct tree *tree_treemake(struct tree *t_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.t = t_data;
	root = treemake(TREE, data, child, args);
	va_end(args);

	return root;
}	

struct tree *void_treemake(struct tree *child, ...) {

	va_list args;
	struct tree *root;
	union data_u data;

	data.t = NULL;

	va_start(args, child);
	root = treemake(VOID, data, child, args);
	va_end(args);

	return root;
}

struct tree *treemake(data_type type, union data_u data, struct tree *child,  va_list args) {
	struct tree *root = malloc(sizeof(struct tree));

	init_tree(root);
	root->type = type;
	root->data = data;


	while (child != NULL) {
		add_child(root, child);
		inc_refcount(child);
		child = va_arg(args, struct tree *);
	}

	return root;
}

int add_child (struct tree *root, struct tree *child) {
	if (root == NULL) {
		return -1;
	}
	inc_refcount(child);
	append(root->children, child);
	root->width += 1;
	return root->width;
}

void set_type (struct tree *root, data_type t) {
	root->type = t;
}

data_type get_type(struct tree *t) {
	return t->type;
}

struct tree *get_branch_t(struct tree *root, struct tree *branch) {
	struct tree *child;
	inc_refcount(branch);
	child = (struct tree *)get_branch(root, branch->data.i);
	dec_refcount(branch);
	return child;
}


struct tree *get_branch(struct tree *root, int branch) {
	return (struct tree *)get(root->children, branch);
}
"

let ll_c =
"

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

void init_list(struct List *list) {
	list->head = NULL;
	list->tail = NULL;
}

void *get(struct List *list, int n) {
	int i = 0;
	struct node *curr;

	curr = list->head;

	while (i < n && curr != NULL) {
		curr = curr->next;
		i++;
	}

	return curr->data;
}




void traverse_list(struct List *list, void (*f)(void *)) 
{
	//Start at the beginning
	struct node *curr = list->head;

	//Iterate until we get to the end of the list
	while (curr != NULL)
	{   
		f(curr->data);
		    
		//Move to next node
		curr = curr->next;
	}   
}


void free_list(struct List *list) {
	while (list->head != NULL) {
		struct node *curr = list->head;
		struct node *next = curr->next;
		free(curr);
		list->head = next;
	}
	free(list);
}



void append(struct List *list, void *data) {
	struct node *curr;
	struct node *new;

	curr = list->tail;

	if (curr == NULL) {
		curr = malloc(sizeof(struct node));
		curr->next = NULL;
		curr->data = data;
		list->head = curr;
		list->tail = curr;
	} else {
		new = malloc(sizeof(struct node));

		new->data = data;
		
		new->next = NULL;

		curr->next = new;

		list->tail = new;
	}
}
"
