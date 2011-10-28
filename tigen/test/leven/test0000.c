#include <stdio.h>

extern int levenshtein(char *s , int ls , char *t , int lt ) ;


int main() {
	char * s;
	int  ls;
	char * t;
	int  lt;
	alarm(2);
	ls = 0;


	levenshtein(s, ls, t, lt);
	return 0;
}
