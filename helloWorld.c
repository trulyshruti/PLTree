#include <stdio.h>
#include <stdlib.h>
#include "tree.h"
void print(char *str){printf("%s", str);}
int main(int argc, char **argv) {
	int a = 0;
	while ((a) < (10)) { 
		print("Hello, ");
		print("World!\n");
		a = (a) + (1);;

	}
}
