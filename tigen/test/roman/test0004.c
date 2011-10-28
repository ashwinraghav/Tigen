#include <stdio.h>

extern void roman(char *s , unsigned int n ) ;


int main() {
	char * s;
	unsigned int  n;
	alarm(2);
	n = 1611;


	roman(s, n);
	return 0;
}
