#include <stdio.h>

extern int dot_product(int *a , int *b , int n ) ;


int main() {
	int * a;
	int * b;
	int  n;
	alarm(2);
	n = 2;


	dot_product(a, b, n);
	return 0;
}
