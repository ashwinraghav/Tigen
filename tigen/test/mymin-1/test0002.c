#include <stdio.h>

extern int mymin(int a , int b , int c ) ;


int main() {
	int  a;
	int  b;
	int  c;
	alarm(2);
	a = 0;
	b = 0;
	c = 0;


	mymin(a, b, c);
	return 0;
}
