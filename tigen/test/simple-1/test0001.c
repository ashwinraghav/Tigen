#include <stdio.h>

extern int simple(int x ) ;


int main() {
	int  x;
	alarm(2);
	x = 0;


	simple(x);
	return 0;
}
