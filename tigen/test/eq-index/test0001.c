#include <stdio.h>

extern int eq_idx(int *a , int len , int **ret ) ;


int main() {
	int * a;
	int  len;
	int ** ret;
	alarm(2);
	len = 2;


	eq_idx(a, len, ret);
	return 0;
}
