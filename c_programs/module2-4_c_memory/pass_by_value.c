// C program to illustrate call by value
#include <stdio.h>

// Function Prototype
void swapx(int x, int y);

// Main function
int main()
{
    int a = 10, b = 20;

    // Pass by Values
    swapx(a, b); // Actual Parameters

    printf("In the caller:\n a = %d b = %d\n", a, b);

    return 0;
}

// Swap functions that swaps two values
void swapx(int x, int y) // Formal Parameters
{
    int t;

    t = x;
    x = y;
    y = t;

    printf("Inside the (callee) function:\n x = %d y = %d\n", x, y);
}
