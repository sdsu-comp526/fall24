#include <stdio.h>

int main() {

    int var = 20; // an int variable declaration
    int *iptr;    // pointer variable declaration
    iptr = &var;  // store address of var in pointer variable
    printf("Address of var variable: %p \n", &var); //address stored in pointer variable
    printf("Address stored in iptr variable: %p\n", iptr); // access the value using the pointer
    printf("Value of ∗iptr variable: %d \n", *iptr); // what iptr points to
    printf("Value of int variable: %d \n", var); // just the plain int variable

    // NULL pointer
    int *ptr = NULL;
    printf("The value of ptr is: %p \n", ptr);

    // print arrays
    int array[] = {42, 77, 89};
    int *array_ptr = array;
    printf("First element of array: %i\n", *(array_ptr++));
    printf("Second element of array: %i\n", *(array_ptr++));
    printf("Third element of array: %i\n", *array_ptr);

    return 0;
}
