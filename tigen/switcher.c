#include <stdio.h>
int main()
{
        simple(10);
}
int simple(int a)
{
        switch(a){
        case 1:
                printf("A is one");
                break;
        case 2:
                printf("A is two");
                break;
        case 10:
                printf("A is ten");
                break;
	}
        /*if(a==10)
                printf("It is ten after all");
        else 
                printf("drat");
        return 0;*/
}
