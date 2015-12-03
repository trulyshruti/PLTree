#include <stdio.h>
#include <stdlib.h>
#include "tree.h"
int main(int argc, char **argv) {
	struct tree * h = char_treemake('h', NULL);;
	struct tree * i = char_treemake('i', NULL);;
	struct tree * nl = char_treemake('\n', NULL);;
	print(
		void_treemake(
			h, 
			i, 
			nl, 
			NULL));
	struct tree * str = tree_treemake(char_treemake('a', NULL), 
		char_treemake('h', NULL), 
		char_treemake('e', NULL), 
		char_treemake('l', NULL), 
		char_treemake('l', NULL), 
		char_treemake('o', NULL), 
		char_treemake('\n', NULL), 
		NULL);;
	print(
		void_treemake(
			char_treemake('H', NULL), 
			char_treemake('e', NULL), 
			char_treemake('l', NULL), 
			char_treemake('l', NULL), 
			char_treemake('o', NULL), 
			char_treemake(',', NULL), 
			char_treemake(' ', NULL), 
			char_treemake('W', NULL), 
			char_treemake('o', NULL), 
			char_treemake('r', NULL), 
			char_treemake('l', NULL), 
			char_treemake('d', NULL), 
			char_treemake('!', NULL), 
			char_treemake('\n', NULL), 
			NULL));
	print(
		str);
	struct tree * a = int_treemake(0, NULL);;
	while (lt(a, int_treemake(10, NULL))) { 
		print(
			tree_treemake(char_treemake('a', NULL), 
				a, 
				char_treemake(' ', NULL), 
				add(a, int_treemake(1, NULL)), 
				NULL));
		print(
			void_treemake(
				char_treemake(':', NULL), 
				char_treemake(' ', NULL), 
				char_treemake('H', NULL), 
				char_treemake('i', NULL), 
				char_treemake('!', NULL), 
				char_treemake('\n', NULL), 
				NULL));
		a = add(a, int_treemake(1, NULL));;

	};
	return 0;
}
