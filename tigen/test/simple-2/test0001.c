#include <stdio.h>

extern int simple(int x , int y ) ;


int main() {
	int  x;
	int  y;
	alarm(2);
	x = 8;
	y = 0;


	simple(x, y);
	return 0;
}
