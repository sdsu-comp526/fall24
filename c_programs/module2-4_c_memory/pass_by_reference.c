// C program to illustrate Call by Reference
#include <stdio.h>

// Function declaration (or prototype/signature)
void swapx(int*, int*);

// Main function
int main()
{
    int a = 10, b = 20;

    // Pass by reference
    swapx(&a, &b); // Actual Parameters. Note that we pass directly the addresses of these two variables

    printf("In the caller:\n a = %d b = %d\n", a, b);

    return 0;
}

// Function to swap two variables by reference
void swapx(int* x, int* y) // Formal Parameters
{
    int t;

    t = *x;
    *x = *y;
    *y = t;

    printf("Inside the (callee) function:\n x = %d y = %d\n", *x, *y);
}
