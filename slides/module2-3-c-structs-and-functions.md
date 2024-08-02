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
