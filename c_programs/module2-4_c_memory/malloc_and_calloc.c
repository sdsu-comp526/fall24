// C code that demonstrates the difference
// between calloc and malloc
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Both of these allocate the same number of bytes,
    // which is the amount of bytes that is required to
    // store 5 int values.

    // The memory allocated by calloc will be
    // zero-initialized, but the memory allocated with
    // malloc will be uninitialized so reading it would be
    // undefined behavior.
    int *allocated_with_malloc = malloc(5 * sizeof(int));
    int *allocated_with_calloc = calloc(5, sizeof(int));

    // As you can see, all of the values are initialized to
    // zero.
    printf("Values of allocated_with_calloc: ");
    for (size_t i = 0; i < 5; ++i) {
        printf("%d ", allocated_with_calloc[i]);
    }
    putchar('\n');

    // This malloc requests 1 terabyte of dynamic memory,
    // which is unavailable in this case, and so the
    // allocation fails and returns NULL.
    int *failed_malloc = malloc(1000000000000);
    if (failed_malloc == NULL) {
        printf("The allocation failed, the value of "
               "failed_malloc is: %p \n",
               (void*)failed_malloc);
    }

    // Remember to always free dynamically allocated memory.
    free(allocated_with_malloc);
    free(allocated_with_calloc);
}
