#include <stdio.h>

extern void bubbleSort(int *numbers , int array_size ) ;


int main() {
	int * numbers;
	int  array_size;
	alarm(2);
	array_size = 0;


	bubbleSort(numbers, array_size);
	return 0;
}
