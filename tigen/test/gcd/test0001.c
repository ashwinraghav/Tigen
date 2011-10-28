#include <stdio.h>

extern int gcd(int m , int n ) ;


int main() {
	int  m;
	int  n;
	alarm(2);
	m = 1;
	n = 0;


	gcd(m, n);
	return 0;
}
