#include <stdio.h>

int main(){

    int var;
    int *ptr;
    int **pptr;

    var = 3000;

    // let ptr to point to the address of var (using the address-of operator &)
    ptr = &var;
    // let pptr to point to the address of ptr (using the address-of operator &)
    pptr = &ptr;

    printf("Value of var = %d \n", var);
    printf("Value pointed to by ptr: *ptr = %d \n", *ptr);
    printf("Value pointed to by pptr: ∗∗pptr = %d \n", **pptr);
    // they are all pointing to the same value!
    return 0;
}
