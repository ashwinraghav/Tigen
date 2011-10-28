#include <stdio.h>

extern int doSqnc(int m ) ;


int main() {
	int  m;
	alarm(2);
	m = 4;


	doSqnc(m);
	return 0;
}
