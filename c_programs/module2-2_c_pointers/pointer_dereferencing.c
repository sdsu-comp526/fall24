#include <stdio.h>

int main (void)
{
    int i = 5;       // i is an int, its value is 5
    int *i_ptr = &i; // i_ptr is a pointer to an int, and we are assigning the address of i to it
    int j = *i_ptr;  // the value of the object pointed to by i_ptr is assigned to j, so j=5
    double *d = &j;  // invalid! We had declared j to be of the type int. We cannot assign it to a pointer of the type double. You will receive a compiler warning
    double d1 = 2.7, d2 = 3.1; // declare two double variables
    double *p = &d1;  // p points to d1, now ∗p = 2.7
    double a = *p;    // the object p points to (2.7) is assigned to a, hence, a = 2.7

    p = &d2;          // p now points to d2, *p = 3.1
    double b = *p;    // decalre a new double variable b and assign the object pointed to by p to it, hence, b = 3.1

    *p = 5.5;         // change the value of the object p points-to to 5.5
    double c = *p;    // and assign it to a newly declared double variable c, hence c = 5.5

    printf("c is %f\n", c);
    printf("d2 is %f\n", d2); // d2 = 5.5,  since we changed what p points to in *p = 5.5.
    /* Careful! Even though we haven't directly changed the value of d2, we have changed it indirectly! */
    return 0;
}
