#include <stdio.h>

int main (void)
{
    int integerVar = 230;
    float floatVar = 626.32;
    double doubleVar = 3.24e+3;
    char charVar = 'a';
    _Bool boolVar = 0;

    printf("integer variable= %i \n",integerVar);
    printf("float variable = %f \n", floatVar);
    printf("double variable = %e \n", doubleVar);
    printf("char variable = %c \n", charVar);
    printf("boolean variable = %i \n", boolVar);

    return 0;
}
