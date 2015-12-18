CC=gcc

.PHONY: all
all: atest hello test_tree gcd

gcd: gcd.c

gcd.c: gcd_tester.tree
	pltree gcd_tester.tree gcd.c

atest: atest.c

atest.c: atest.tree
	pltree atest.tree atest.c

hello: hello.c

hello.c: hello.tree
	pltree hello.tree hello.c
test_tree: test_tree.c

test_tree.c: test_tree.tree
	pltree test_tree.tree test_tree.c
.PHONY: clean

clean:
	rm -rf *.c *.tmp gcd atest hello test_tree
