#include <stdio.h>

extern int simple(int x , int y ) ;


int main() {
	int  x;
	int  y;
	alarm(2);
	x = 7;
	y = 4;


	simple(x, y);
	return 0;
}
