#include <stdio.h>

extern int simple(int x , int y ) ;


int main() {
	int  x;
	int  y;
	alarm(2);
	x = 66;
	y = 66;


	simple(x, y);
	return 0;
}
