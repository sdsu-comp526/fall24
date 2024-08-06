# 6) C structs and functions

Today:
 1. Structs
 2. Functions

## 1. Structs
- We saw that arrays allow to define type of variables that can hold several data items of the _same kind_
- `struct`s (or structures) are user-defined data type that allow to collect data items (members) of _different kinds_

### Definition
Pseudo-code:

```
struct[structure tag]
{
    member definition;
    ...
    member definition;

} [one or more structure variables]
```

Example:

```c
struct Books
{
    char title[50];
    char author[50];
    char subject[100];
    int year;
    double price;
} book;
```

- A member of a structure is accessed by specifying the variable name, followed by a period, and the member name. Example:

```c
book.price = 49.99;
```

- Initializing structures:

```c
struct Books cBook = {"Programming in C", "Steve G. Kochan", " Programming languages", 2004, 49.95}; // initialize all members (in order)

struct Books cBook = {.author = "Steve G. Kochan", .year = 2004}; // initialize only specific members with the dot operator
```

- Structs can contain struct members (nested structs)

Example:

```c
#include <stdio.h>
#include <string.h>

// inner (or dependend) structure
struct Employee
{
    int employee_id;
    char name[20];
    int salary;
};
// outer structure
struct Organization
{
    char orgName[20];
    char orgNumber[20];
    struct Employee emp;
};

int main(void)
{
    ...

    struct Organization org;
    // access members of the inner struct by using the dot operator twice
    org.emp.employee_id = 153;
    org.enp.salary = 60000;

    return 0;
}
```

- One can have a collection of structs like an array of structs. Example:

```c
struct car
{
    char make[20];
    char model[30];
    int year;
};
struct car arr_car[10];

// assign the model member of the 3rd element of the array of structs
arr_car[2].model = "Legacy";

```


## 2. Functions

Most of this section material is adopted from this [resource](http://www.faqs.org/docs/learnc/c178.html).

_Functions_ can be thought of as named blocks of code that (if good software design principles are followed) perform one task. When you write a program you will write many functions to perform the tasks you need. There are, however, a lot of common tasks such as displaying text to the screen that a lot of programmers will need. Instead of having everyone reinventing the wheel, GNU systems (Unix-like operating system) come with libraries of pre-defined functions for many of these tasks. Over the years, thousands of such functions have accumulated.

Every C program must have a function called `main()`, this is where execution of the program begins. In principle, the code of a program could be completely contained in `main()` but it is more usual and practical to split a program into many small functions that perform single tasks.

First you must _define_ the function, just like we defined `main()` in the examples so far. Also you you must `declare` it. Declaring a function is like telling the compiler to expect it, we didn't have to declare `main()` because it is a special function and the compiler knows to expect it. The name, or identifier, you give to a function must appear in both the definition and the declaration.

- Functions identifiers can be made up of the alphabetic characters "a"-"z" and "A"-"Z", the numeric characters "0"-"9" and the underscore character "_". These can be used in any order so long as the first character of the identifier is not a number. As for variables, C is **case-sensitive** so `My_Function` is completely different to `my_function`. A function identifier must be unique. Identifiers can safely be up to 63 characters long or as short as 1 character.

Along with it's identifier you must give each function a _type_ and a block of code. The _type_ tells the compiler what sort of data it _returns_. The return value of a function can be ignored. For instance, `printf()` returns an integer saying how many character it displayed to the terminal, but if it worked correctly, i.e., it printed something to the standard output (your screen) then it is not super helpful to know that value (but it can be for debugging purposes).

Example:

```{literalinclude} ../c_programs/module2-3_c_functions/functions.c
:language: c
:linenos: true
```

In the above example we wrote `first_function()`, which does nothing and `goodbye()`, which displays a message. Functions must be declared _before_ they are called. In our case this means they must appear before our definition of `main()`. In practice, function declarations are generally grouped at the top of a file after any `#include` lines and before any function definitions or can be in their own header file included with the `#include` directive.

- Keeping track of function declarations can get messy, for this reason _header files_ are used to house C code that you wish to appear in multiple files. You have actually already used a header file. `stdio.h` is a header file which contains many function declarations, it contains the function declarations for `printf()` and `printf()`. Once you have placed the function declarations you wish to share into a header file you can `#include` your header in each C file that needs the information. The only difference being that you surround your filename in quotes instead of angle brackets (`"my_header.h"` instead of `<system_header.h>`).
