#include <stdio.h>

int main (void)
{
    int array[] = {42, 77, 89};
    printf("Array %p\n", array); // it will print some hexadecimal string like 0x7fffa66c0fac

    // print array values in a for-loop
    int i, *ptr;
    ptr = array; // this way the address of the first entry of array is stored in ptr

    for (i=0; i<3; i++) {
        printf("Address of array [%d] = %p \n", i, ptr);
        printf("Value of array [%d] = %i \n", i, *ptr);

        // point to the next location
        ptr++;
    }

    return 0;
}
