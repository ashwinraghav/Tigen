#include <stdio.h>

extern int hailstone(int i ) ;


int main() {
	int  i;
	alarm(2);
	i = 2;


	hailstone(i);
	return 0;
}
