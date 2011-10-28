#include <stdio.h>

extern int result(float a , float b ) ;


int main() {
	float  a;
	float  b;
	alarm(2);
	a = 0;
	b = 0;


	result(a, b);
	return 0;
}
