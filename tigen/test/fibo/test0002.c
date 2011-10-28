#include <stdio.h>

extern int fib(int n ) ;


int main() {
	int  n;
	alarm(2);
	n = 4;


	fib(n);
	return 0;
}
