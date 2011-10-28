#include <stdio.h>

extern int result(float a , float b ) ;


int main() {
	float  a;
	float  b;
	alarm(2);
	a = 0;
	b = -1;


	result(a, b);
	return 0;
}
