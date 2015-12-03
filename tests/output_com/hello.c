#include <stdio.h>
#include <stdlib.h>
#include "tree.h"
int main(int argc, char **argv) {
	struct tree * str = void_treemake(
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
	return 0;
}
