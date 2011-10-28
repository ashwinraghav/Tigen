#include <stdio.h>

extern int levenshtein(char *s , int ls , char *t , int lt ) ;


int main() {
	char * s;
	int  ls;
	char * t;
	int  lt;
	alarm(2);
	ls = 2;
	lt = 1;


	levenshtein(s, ls, t, lt);
	return 0;
}
