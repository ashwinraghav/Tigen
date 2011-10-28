#include <stdio.h>

extern int simple(int x , int y ) ;


int main() {
	int  x;
	int  y;
	alarm(2);
	x = 25;
	y = 1;


	simple(x, y);
	return 0;
}
