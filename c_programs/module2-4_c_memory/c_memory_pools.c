#include <stdio.h>
#include <stdlib.h>

int x; // global variable, stored in the static pool

int main(void)
{
    int y;      // dynamic stack storage
    char *str;  // pointer to type char

    y = 4;
    printf("stack memory: %d\n", y);

    str = malloc(100*sizeof(char)); // create heap storage (a contiguous block) and assign its address to str
    str[0] = 'm'; // str can be seen as an array, and we assign the character 'm' to its first entry
    printf("heap memory: %c\n", str[0]);
    free(str);
    return 0;
}
