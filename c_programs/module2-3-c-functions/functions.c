#include <stdio.h>

/* function declarations */
int first_function(void);
int goodbye(void);


int
main()            // function definition
{
  printf("the program begins...\n");
  first_function();
  goodbye();

  return 0;
}


int
first_function()  // function definition
{
  /* this function does nothing */
  return 0;
}


int
goodbye()         // function definition
{
  printf("...and the program ends.\n");

  return 0;
}
